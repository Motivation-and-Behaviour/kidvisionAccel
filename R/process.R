#' Process recordings with GGIR
#'
#' Step 3 of the workflow. Runs GGIR, with the settings in [ggir_params()], on
#' each recording copied by [kv_fetch()].
#'
#' Each recording is processed in its own R process and a new, empty output
#' folder, so a file that crashes GGIR, or takes longer than `timeout`
#' minutes, stops only that recording. Up to `workers` recordings are
#' processed at once. GGIR's messages are saved to
#' `logs/<record_id>_<timepoint>_ggir.log` in the work folder.
#'
#' When the visit dates are plausible (visit 2 is 5 to 21 days after visit 1,
#' and the recording started on visit 1 or up to 3 days before), GGIR uses
#' only the data from visit 1 to visit 2. This leaves out time before the
#' device was handed over and after it was collected.
#'
#' GGIR doesn't always stop with an error when it can't process a file, so
#' success is judged from its output: all the expected result files must
#' exist, GGIR's quality report must not flag the file as corrupt, and the
#' participant ID in the results must be the record ID, with one recording.
#'
#' @param jobs A job table from [kv_fetch()].
#' @param config A [kv_config()] object.
#' @param timeout Minutes to allow GGIR for each recording. A 9-day
#'   recording usually takes 3 to 5 minutes.
#' @param workers How many recordings to process at once. Each needs about
#'   3 GB of memory.
#'
#' @return The job table, with GGIR's output folder in `ggir_dir` and the
#'   period it used in `study_first` and `study_last` (`NA` for the whole
#'   recording). Recordings GGIR processed have status `"processed"`; the
#'   others have status `"ggir_failed"`, `"invalid_file"`, `"needs_review"` or
#'   `"no_valid_days"`.
#' @export
kv_process <- function(jobs, config = kv_config(), timeout = 60,
                       workers = config$workers) {
  if (!"ggir_dir" %in% names(jobs)) jobs$ggir_dir <- NA_character_
  if (!"study_first" %in% names(jobs)) jobs$study_first <- as.Date(NA)
  if (!"study_last" %in% names(jobs)) jobs$study_last <- as.Date(NA)
  rows <- which(jobs$status == "ready")
  log_dir <- file.path(config$work_dir, "logs")
  fs::dir_create(log_dir)

  runs <- list()
  for (i in rows) {
    prepared <- prepare_ggir(jobs, i, config, log_dir)
    jobs <- prepared$jobs
    runs[[length(runs) + 1]] <- prepared$run
  }
  errors <- run_ggir_jobs(runs, workers, timeout)

  for (k in seq_along(rows)) {
    i <- rows[[k]]
    jobs$ggir_dir[[i]] <- runs[[k]]$ggir_dir
    outcome <- check_ggir_output(
      runs[[k]]$ggir_dir, jobs$record_id[[i]], nrow(jobs$files[[i]]),
      errors[[k]], runs[[k]]$log
    )
    jobs <- set_status(jobs, i, outcome$status, outcome$message)
  }

  report_jobs(jobs, "GGIR", rows)
}

job_name <- function(jobs, i) {
  paste0(jobs$record_id[[i]], "_", jobs$timepoint[[i]])
}

# Sets up GGIR's output folder and arguments for job `i`. Returns the updated
# job table and the run: GGIR's arguments, log file and output folder.
prepare_ggir <- function(jobs, i, config, log_dir) {
  name <- job_name(jobs, i)
  out_dir <- file.path(jobs$job_dir[[i]], "ggir")
  if (fs::dir_exists(out_dir)) fs::dir_delete(out_dir)
  fs::dir_create(out_dir)

  files <- jobs$files[[i]]
  args <- c(
    list(datadir = files$local, outputdir = out_dir, studyname = name),
    ggir_params(config$tz)
  )
  dates <- study_dates(
    jobs$visit1_date[[i]], jobs$visit2_date[[i]], min(files$header_start)
  )
  if (is.na(dates$reason)) {
    path <- file.path(jobs$job_dir[[i]], "study_dates.csv")
    utils::write.csv(
      data.frame(
        ID = jobs$record_id[[i]], first = format(dates$first),
        last = format(dates$last)
      ),
      path,
      row.names = FALSE
    )
    args$study_dates_file <- path
    args$study_dates_dateformat <- "%Y-%m-%d"
    jobs$study_first[[i]] <- dates$first
    jobs$study_last[[i]] <- dates$last
    jobs <- add_note(jobs, i, glue::glue(
      "Used data from {dates$first} to {dates$last} (visit 1 to visit 2)."
    ), check = FALSE)
  } else {
    jobs <- add_note(jobs, i, glue::glue(
      "Used the whole recording, as {dates$reason}."
    ))
  }

  run <- list(
    name = name,
    args = args,
    log = file.path(log_dir, paste0(name, "_ggir.log")),
    ggir_dir = file.path(out_dir, paste0("output_", name))
  )
  list(jobs = jobs, run = run)
}

# Runs GGIR for each run, `workers` at a time, each in its own R process.
# Returns, for each run, NULL if GGIR finished or the error it stopped with.
run_ggir_jobs <- function(runs, workers, timeout) {
  errors <- vector("list", length(runs))
  queue <- seq_along(runs)
  running <- integer()
  processes <- list()
  started <- numeric(length(runs))

  while (length(queue) > 0 || length(running) > 0) {
    while (length(running) < workers && length(queue) > 0) {
      k <- queue[[1]]
      queue <- queue[-1]
      cli::cli_inform(c(
        "i" = "Running GGIR on {runs[[k]]$name} ({k} of {length(runs)})."
      ))
      processes[[k]] <- start_ggir(runs[[k]]$args, runs[[k]]$log)
      started[[k]] <- as.numeric(Sys.time())
      running <- c(running, k)
    }
    for (k in running) {
      process <- processes[[k]]
      if (!process$is_alive()) {
        errors[k] <- list(tryCatch(
          {
            process$get_result()
            NULL
          },
          error = function(e) e
        ))
        running <- setdiff(running, k)
      } else if (as.numeric(Sys.time()) - started[[k]] > timeout * 60) {
        process$kill_tree()
        errors[[k]] <- structure(
          class = c("callr_timeout_error", "error", "condition"),
          list(message = "GGIR timed out", call = NULL)
        )
        running <- setdiff(running, k)
      }
    }
    if (length(running) > 0) Sys.sleep(0.5)
  }
  errors
}

# Starts GGIR in a background R process, with its messages going to `log`.
start_ggir <- function(args, log) {
  callr::r_bg(
    function(args) do.call(GGIR::GGIR, args),
    args = list(args = args),
    stdout = log,
    stderr = "2>&1",
    supervise = TRUE
  )
}

# Judges whether GGIR processed a recording, from its output and any error.
# Returns a list with `status` and `message`.
check_ggir_output <- function(ggir_dir, record_id, n_files, error, log) {
  outcome <- function(status, message = NA_character_) {
    list(status = status, message = message)
  }
  see_log <- paste0(" See ", log, ".")

  if (inherits(error, "callr_timeout_error")) {
    return(outcome(
      "ggir_failed", paste0("GGIR didn't finish in time.", see_log)
    ))
  }
  if (!is.null(error)) {
    cause <- if (is.null(error$parent)) error else error$parent
    message <- strsplit(conditionMessage(cause), "\n")[[1]]
    return(outcome("ggir_failed", paste0(
      "GGIR stopped with an error ", ggir_stage(ggir_dir), ": ",
      message[nzchar(trimws(message))][1], ".", see_log
    )))
  }

  quality <- ggir_result_path(ggir_dir, "quality")
  if (!is.na(quality)) {
    report <- read_ggir_csv(quality)
    for (flag in c("file.corrupt", "file.too.short")) {
      if (any(report[[flag]] %in% "TRUE")) {
        return(outcome("invalid_file", glue::glue(
          "GGIR's quality report flags {report$filename[report[[flag]] %in% ",
          "'TRUE'][1]} as {sub('file.', '', flag, fixed = TRUE)}."
        )))
      }
    }
  }

  paths <- vapply(names(ggir_results_files), ggir_result_path, "",
    ggir_dir = ggir_dir
  )
  if (!is.na(paths[["part2_day"]])) {
    days <- read_ggir_csv(paths[["part2_day"]])
    if (!any(as_number(days$`N valid hours`) >= 16, na.rm = TRUE)) {
      return(outcome(
        "no_valid_days", "No day had the 16 hours of wear needed."
      ))
    }
  }
  missing <- names(paths)[is.na(paths)]
  if (length(missing) > 0) {
    return(outcome("ggir_failed", paste0(
      "GGIR stopped ", ggir_stage(ggir_dir), " without writing ",
      paste(ggir_results_files[missing], collapse = ", "), ".", see_log
    )))
  }

  ids <- unique(days$ID)
  if (!identical(ids, record_id)) {
    return(outcome("needs_review", glue::glue(
      "GGIR read the participant ID as {paste(ids, collapse = ', ')}, ",
      "not {record_id}."
    )))
  }
  recordings <- unique(days$filename)
  if (length(recordings) > 1) {
    return(outcome("needs_review", glue::glue(
      "GGIR processed the {n_files} files as separate recordings instead of ",
      "appending them: {paste(recordings, collapse = ', ')}."
    )))
  }

  outcome("processed")
}

# The GGIR result files used, by name. Part 5's file names include the
# intensity thresholds, so they are matched by pattern.
ggir_results_files <- c(
  part2_day = "part2_daysummary.csv",
  part2_person = "part2_summary.csv",
  part4_night = "part4_nightsummary_sleep_cleaned.csv",
  part4_person = "part4_summary_sleep_cleaned.csv",
  part5_mm = "part5_daysummary_MM_*.csv",
  part5_ww = "part5_daysummary_WW_*.csv",
  part6 = "part6_summary.csv"
)

# Path to one of GGIR's result files, or NA if it doesn't exist.
ggir_result_path <- function(ggir_dir, file) {
  pattern <- if (file == "quality") {
    "QC/data_quality_report.csv"
  } else {
    ggir_results_files[[file]]
  }
  found <- Sys.glob(file.path(ggir_dir, "results", pattern))
  if (length(found) == 1) found else NA_character_
}

# Describes how far GGIR got, from the milestone folder each part writes.
ggir_stage <- function(ggir_dir) {
  milestones <- c(
    "basic", "ms2.out", "ms3.out", "ms4.out", "ms5.out", "ms6.out"
  )
  done <- vapply(milestones, function(m) {
    folder <- file.path(ggir_dir, "meta", m)
    fs::dir_exists(folder) && length(fs::dir_ls(folder)) > 0
  }, logical(1))
  if (!any(done)) {
    return("in part 1")
  }
  paste("after part", max(which(done)))
}

read_ggir_csv <- function(path) {
  utils::read.csv(
    path,
    colClasses = "character", check.names = FALSE, na.strings = c("", "NA")
  )
}

as_number <- function(x) suppressWarnings(as.numeric(x))

# The period GGIR should use: visit 1 to visit 2, if those dates are
# plausible. Otherwise `reason` says why the whole recording is used.
study_dates <- function(visit1, visit2, start, window_days = 3) {
  skip <- function(reason) list(first = NA, last = NA, reason = reason)
  if (is.na(visit1) || is.na(visit2)) {
    return(skip("a visit date is missing"))
  }
  days <- as.numeric(visit2 - visit1)
  if (days < 5 || days > 21) {
    return(skip(glue::glue("visit 2 was {days} days after visit 1")))
  }
  start <- as.Date(format(start, "%Y-%m-%d"))
  if (is.na(start) || start < visit1 - window_days || start > visit1) {
    return(skip(glue::glue(
      "the recording started on {start}, but visit 1 was {visit1}"
    )))
  }
  list(first = visit1, last = visit2, reason = NA_character_)
}
