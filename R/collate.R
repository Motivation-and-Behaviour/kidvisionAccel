#' Merge GGIR's results and prepare the REDCap values
#'
#' Step 4 of the workflow, part 1. For each recording processed by
#' [kv_process()]:
#' * writes one CSV per GGIR part to upload to REDCap: the part 2 day
#'   summary, the part 4 night summary, the part 5 day summaries for both the
#'   midnight-to-midnight (`MM`) and waking-to-waking (`WW`) windows (with a
#'   `window` column), and the part 6 summary. Each has the record ID, REDCap
#'   event and software versions added;
#' * builds a table with one row per measurement day (see Details);
#' * prepares the values for REDCap's accelerometer instruments.
#'
#' Measurement day 1 is the first calendar date of the period GGIR used: visit
#' 1 when the visit dates were used, or else the first day of the recording.
#' Each day has the hours recorded and valid from part 2, physical activity
#' from part 5's midnight-to-midnight window, and sleep from the part 4 night
#' that starts on the evening of that day. Activity is only given for valid
#' days (at least 16 hours of wear, and two-thirds of waking hours), and sleep
#' only for valid nights, as defined by GGIR. Averages are over valid days or
#' nights; weekdays are Monday to Friday.
#'
#' REDCap has room for 10 days. Any later days are in the uploaded files but
#' not in REDCap's day fields, and a note is added to the recording.
#'
#' @param jobs A job table from [kv_process()].
#' @param config A [kv_config()] object.
#'
#' @return The job table, with the CSV files to upload in `part_files`, the
#'   day table in `days`, and the REDCap values in `redcap` (a one-row data
#'   frame, also saved as `<record_id>_<timepoint>_redcap.csv` with the part
#'   files). Recordings have status `"collated"`, or `"no_valid_days"` if
#'   there was no valid day or night.
#' @export
kv_collate <- function(jobs, config = kv_config()) {
  for (column in c("part_files", "days", "redcap")) {
    if (!column %in% names(jobs)) jobs[[column]] <- rep(list(NULL), nrow(jobs))
  }

  rows <- which(jobs$status == "processed")
  for (i in rows) {
    jobs <- tryCatch(
      collate_job(jobs, i, config),
      error = function(e) {
        set_status(jobs, i, "ggir_failed", paste(
          "GGIR's results couldn't be merged:", conditionMessage(e)
        ))
      }
    )
  }

  report_jobs(jobs, "Collate", rows)
}

collate_job <- function(jobs, i, config) {
  name <- job_name(jobs, i)
  results <- read_ggir_results(jobs$ggir_dir[[i]])
  days <- day_table(results, jobs$study_first[[i]], jobs$study_last[[i]])
  jobs$days[i] <- list(days)
  if (!any(days$valid_day) && !any(days$valid_night)) {
    return(set_status(
      jobs, i, "no_valid_days", "No day or night had enough wear to use."
    ))
  }

  out_dir <- file.path(jobs$job_dir[[i]], "out")
  fs::dir_create(out_dir)
  provenance <- tibble::tibble(
    record_id = jobs$record_id[[i]],
    redcap_event_name = jobs$event[[i]],
    ggir_version = results$part2_person$`GGIR version`[[1]],
    ggirread_version = installed_version("GGIRread"),
    kidvisionaccel_version = installed_version("kidvisionAccel"),
    processed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = config$tz)
  )
  part_files <- write_part_files(results, provenance, out_dir, name)
  jobs$part_files[i] <- list(part_files)

  record <- to_redcap_record(
    days, results, jobs$record_id[[i]], jobs$event[[i]]
  )
  utils::write.csv(
    record, file.path(out_dir, paste0(name, "_redcap.csv")),
    row.names = FALSE, na = ""
  )
  jobs$redcap[i] <- list(record)

  if (nrow(days) > n_measurement_days) {
    jobs <- add_note(jobs, i, glue::glue(
      "The recording covers {nrow(days)} days; only the first ",
      "{n_measurement_days} are in REDCap's day fields."
    ))
  }
  set_status(jobs, i, "collated")
}

read_ggir_results <- function(ggir_dir) {
  paths <- vapply(
    names(ggir_results_files), ggir_result_path, "",
    ggir_dir = ggir_dir
  )
  if (anyNA(paths)) {
    stop("missing ", paste(ggir_results_files[is.na(paths)], collapse = ", "))
  }
  lapply(paths, read_ggir_csv)
}

# Writes the files uploaded to REDCap, one per GGIR part, and returns their
# paths named by part.
write_part_files <- function(results, provenance, out_dir, name) {
  part5 <- dplyr::bind_rows(
    MM = results$part5_mm, WW = results$part5_ww, .id = "window"
  )
  parts <- list(
    part2 = results$part2_day,
    part4 = results$part4_night,
    part5 = part5,
    part6 = results$part6
  )
  paths <- file.path(out_dir, paste0(name, "_", names(parts), ".csv"))
  for (k in seq_along(parts)) {
    data <- dplyr::bind_cols(
      provenance[rep(1, nrow(parts[[k]])), ], parts[[k]]
    )
    utils::write.csv(data, paths[[k]], row.names = FALSE, na = "")
  }
  stats::setNames(paths, names(parts))
}

# One row per measurement day, from `first` to `last` (the period GGIR used;
# NA for the whole recording).
day_table <- function(results, first = NA, last = NA) {
  part2 <- results$part2_day
  date <- as.Date(part2$calendar_date)
  keep <- (is.na(first) | date >= first) & (is.na(last) | date <= last)
  part2 <- part2[keep, ][order(date[keep]), ]
  date <- as.Date(part2$calendar_date)

  days <- tibble::tibble(
    day = seq_along(date),
    date = date,
    weekday = weekday_number(date),
    weekend = weekday_number(date) >= 6,
    hours = as_number(part2$`N hours`),
    valid_hours = as_number(part2$`N valid hours`)
  )

  activity <- results$part5_mm
  a <- match(date, as.Date(activity$calendar_date))
  minutes <- function(column) as_number(activity[[column]])[a]
  days$valid_day <- !is.na(a)
  days$sedentary <- minutes("dur_day_total_IN_min")
  days$light <- minutes("dur_day_total_LIG_min")
  days$moderate <- minutes("dur_day_total_MOD_min")
  days$vigorous <- minutes("dur_day_total_VIG_min")
  days$mvpa <- days$moderate + days$vigorous
  # The rest of the 24-hour window, for the report: sleep and time awake
  # during the sleep period. Windows are 23 or 25 hours on daylight saving
  # changes.
  days$sleep_period_sleep <- minutes("dur_spt_sleep_min")
  days$sleep_period_awake <- minutes("dur_spt_min") - days$sleep_period_sleep
  days$window <- minutes("dur_day_spt_min")

  nights <- results$part4_night
  n <- match(date, as.Date(nights$calendar_date))
  sleep <- function(column) nights[[column]][n]
  days$valid_night <- !is.na(n)
  days$sleep_hours <- as_number(sleep("SleepDurationInSpt"))
  days$sleep_period_hours <- as_number(sleep("SptDuration"))
  days$sleep_efficiency <- days$sleep_hours / days$sleep_period_hours
  days$onset <- as_number(sleep("sleeponset"))
  days$onset_time <- sleep("sleeponset_ts")
  days$wake <- as_number(sleep("wakeup"))
  days$wake_time <- sleep("wakeup_ts")
  days
}

# 1 for Monday to 7 for Sunday, as in REDCap's day-of-the-week fields.
weekday_number <- function(date) {
  as.integer(format(date, "%u"))
}
