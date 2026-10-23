#' Write the results to REDCap
#'
#' Step 6 of the workflow. Records what happened to each recording in REDCap.
#'
#' For a recording with a report, in this order:
#' 1. its processing status is set to "In progress";
#' 2. the values prepared by [kv_collate()] are imported, replacing any from
#'    earlier processing (including with blanks);
#' 3. the part 2, 4, 5 and 6 files and the report are uploaded;
#' 4. its processing status is set to "Complete", or "Complete - check notes"
#'    if there is something worth looking at, with the date, software
#'    versions, GGIR settings, number of files and notes. The accelerometer
#'    physical activity, sleep, time and output instruments and the
#'    participant report are marked complete. The accelerometer details
#'    instrument is left alone, as research staff fill in part of it.
#'
#' If anything fails part-way, the status stays "In progress", so the
#' recording is processed again on the next run.
#'
#' For a recording that couldn't be processed, only its processing status,
#' date and notes are recorded: "No file found" (no folder or file), "No
#' valid days", "Failed (will retry)" (copying or the report failed, or GGIR
#' ran out of time), "Needs manual review" (a misnamed folder, or files that
#' need a decision), or "Failed (cannot be processed)" (GGIR couldn't process
#' the file, or it is unreadable or incomplete).
#'
#' Every set of values is first checked against the REDCap project. A value
#' REDCap wouldn't accept stops that recording, rather than being imported as
#' a blank.
#'
#' @param jobs A job table from [kv_report()].
#' @param config A [kv_config()] object.
#' @param dry_run If `TRUE`, nothing is sent to REDCap. The values and files
#'   that would be are saved in `dry_run/<record_id>_<timepoint>/` in the
#'   work folder instead, after being checked against the REDCap project.
#'
#' @return The job table, with the processing status recorded in REDCap in
#'   `redcap_status`. Recordings with a report have status `"written"`, or
#'   `"write_failed"` if writing failed.
#' @export
kv_write <- function(jobs, config = kv_config(), dry_run = FALSE) {
  if (!"redcap_status" %in% names(jobs)) jobs$redcap_status <- NA_integer_
  rows <- which(!is.na(redcap_status_code(jobs$status, jobs$message)))
  if (length(rows) == 0) {
    return(report_jobs(jobs, "Write", rows))
  }

  rcon <- rc_connect(config)
  for (i in rows) {
    writer <- redcap_writer(
      rcon, config, dry_run,
      record = jobs$record_id[[i]], event = jobs$event[[i]],
      name = job_name(jobs, i)
    )
    jobs <- tryCatch(
      if (jobs$status[[i]] == "reported") {
        write_results(jobs, i, writer, config)
      } else {
        write_status_only(jobs, i, writer, config)
      },
      error = function(e) {
        message <- paste("Writing to REDCap failed:", conditionMessage(e))
        if (jobs$status[[i]] == "reported") {
          set_status(jobs, i, "write_failed", message)
        } else {
          add_note(jobs, i, message)
        }
      }
    )
  }

  report_jobs(jobs, if (dry_run) "Write (dry run)" else "Write", rows)
}

# The REDCap processing status (cac_proc_status) for each job status, or NA
# for jobs that haven't finished.
redcap_status_code <- function(status, message) {
  code <- c(
    reported = 1L, no_folder = 3L, no_files = 3L, no_valid_days = 4L,
    copy_failed = 5L, report_failed = 5L, misnamed = 6L, needs_review = 6L,
    ggir_failed = 7L, invalid_file = 7L
  )[status]
  # Running out of time can be temporary, so is tried again
  timed_out <- status %in% "ggir_failed" &
    grepl("didn't finish in time", message, fixed = TRUE)
  code[timed_out] <- 5L
  unname(code)
}

# Forms marked complete once a recording's results are in REDCap.
completed_forms <- c(
  "accelerometer_physical_activity", "accelerometer_sleep",
  "accelerometer_time", "accelerometer_output", "participant_reports"
)

# The REDCap fields each uploaded file goes into.
file_fields_by_part <- c(
  part2 = "acc_pt2", part4 = "acc_pt4", part5 = "acc_pt5", part6 = "acc_pt6"
)

write_results <- function(jobs, i, writer, config) {
  values <- jobs$redcap[[i]]
  code <- if (jobs$check_notes[[i]]) 2L else 1L
  done <- status_record(jobs, i, code, config)
  for (form in completed_forms) done[[paste0(form, "_complete")]] <- "2"
  writer$validate(values)
  writer$validate(done)

  writer$records(status_record(jobs, i, 0L, config, brief = TRUE), "1_status")
  writer$records(values, "2_values")
  files <- jobs$part_files[[i]]
  for (part in names(file_fields_by_part)) {
    writer$file(files[[part]], file_fields_by_part[[part]])
  }
  writer$file(jobs$report[[i]], "reports_accel")
  writer$records(done, "3_status")

  jobs$redcap_status[[i]] <- code
  set_status(jobs, i, "written")
}

write_status_only <- function(jobs, i, writer, config) {
  code <- redcap_status_code(jobs$status[[i]], jobs$message[[i]])
  record <- status_record(jobs, i, code, config)
  writer$validate(record)
  writer$records(record, "status")
  jobs$redcap_status[[i]] <- code
  jobs
}

# The processing-status fields for job `i`. `brief` gives only the status
# and date, for marking a recording as in progress.
status_record <- function(jobs, i, code, config, brief = FALSE) {
  record <- tibble::tibble(
    record_id = jobs$record_id[[i]],
    redcap_event_name = jobs$event[[i]],
    cac_proc_status = as.character(code),
    cac_proc_datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = config$tz)
  )
  if (brief) {
    return(record)
  }
  files <- job_files(jobs, i)
  record$cac_proc_ggir <- installed_version("GGIR")
  record$cac_proc_pkg <- installed_version("kidvisionAccel")
  record$cac_proc_params <- ggir_params_hash(config$tz)
  record$cac_proc_nfiles <- as.character(if (is.null(files)) 0 else nrow(files))
  record$cac_proc_notes <- processing_notes(jobs, i, config)
  record
}

# What to record about the processing: why it stopped, any notes, and the
# source files used.
processing_notes <- function(jobs, i, config) {
  files <- job_files(jobs, i)
  sources <- if (!is.null(files) && nrow(files) > 0) {
    paste0(
      "Source file: ", fs::path_rel(files$path, config$source_dir), " (",
      round(files$size / 1e6), " MB)"
    )
  }
  lines <- c(stats::na.omit(jobs$message[[i]]), jobs$notes[[i]], sources)
  if (length(lines) == 0) NA_character_ else paste(lines, collapse = "\n")
}

# The files found for job `i`, or NULL if it never got as far as locating
# them.
job_files <- function(jobs, i) {
  if ("files" %in% names(jobs)) jobs$files[[i]] else NULL
}

# Sends values and files to REDCap, or, for a dry run, saves them in the work
# folder. Either way, values are checked against the project first.
redcap_writer <- function(rcon, config, dry_run, record, event, name) {
  validate <- function(data) rc_validate(rcon, data)
  if (!dry_run) {
    return(list(
      validate = validate,
      records = function(data, step) rc_import_records(rcon, data),
      file = function(path, field) {
        rc_import_file(
          rcon, path,
          record = record, field = field, event = event
        )
      }
    ))
  }

  dir <- file.path(config$work_dir, "dry_run", name)
  fs::dir_create(dir)
  list(
    validate = validate,
    records = function(data, step) {
      utils::write.csv(
        data, file.path(dir, paste0(step, ".csv")),
        row.names = FALSE, na = ""
      )
    },
    file = function(path, field) {
      fs::file_copy(
        path, file.path(dir, paste0(field, "_", basename(path))),
        overwrite = TRUE
      )
    }
  )
}
