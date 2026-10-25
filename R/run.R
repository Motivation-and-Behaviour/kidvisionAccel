#' Process accelerometer recordings, from REDCap to REDCap
#'
#' Runs the whole workflow:
#' 1. checks everything needed is in place ([kv_check()]);
#' 2. finds the recordings to process ([kv_pending()]);
#' 3. finds their files on OneDrive ([kv_locate()]);
#' 4. for `workers` recordings at a time: copies their files ([kv_fetch()]),
#'    runs GGIR ([kv_process()]), merges the results ([kv_collate()]),
#'    creates the reports ([kv_report()]), writes everything to REDCap
#'    ([kv_write()]) and deletes the local copies ([kv_cleanup()]).
#'
#' Recordings that can't be processed have their processing status and the
#' reason recorded in REDCap, so they aren't tried again on every run (see
#' [kv_pending()] for when they are).
#'
#' A summary is printed at the end and saved, with one row per recording, to
#' `logs/run_<date and time>.csv` in the work folder. It has no names.
#'
#' @inheritParams kv_pending
#' @param dry_run If `TRUE`, nothing is written to REDCap; see [kv_write()].
#' @param workers How many recordings to process at once; see
#'   [kv_process()].
#' @param cleanup If `FALSE`, the local copies and results are kept in the
#'   work folder.
#'
#' @return Invisibly, the job table, with one row per recording.
#' @export
#' @examples
#' \dontrun{
#' # Everything that hasn't been processed yet
#' kv_run()
#'
#' # Try it out without writing to REDCap
#' kv_run(ids = c("1009", "4340"), dry_run = TRUE)
#'
#' # Reprocess after changing the GGIR version or settings
#' kv_run(reprocess = "outdated")
#' }
kv_run <- function(
  config = kv_config(),
  reprocess = c("none", "outdated", "all"),
  ids = NULL,
  timepoints = NULL,
  dry_run = FALSE,
  workers = config$workers,
  cleanup = TRUE
) {
  reprocess <- rlang::arg_match(reprocess)
  started <- Sys.time()
  kv_check(config, dry_run = dry_run)
  if (dry_run) {
    cli::cli_inform(c("i" = "Dry run: nothing will be written to REDCap."))
  }

  jobs <- kv_pending(config, reprocess, ids, timepoints)
  if (nrow(jobs) == 0) {
    return(invisible(jobs))
  }
  jobs <- kv_locate(jobs, config)
  keys <- paste(jobs$record_id, jobs$timepoint)

  # Recordings that stopped at locate only need their status recording
  stopped <- jobs[jobs$status != "located", ]
  done <- list()
  if (nrow(stopped) > 0) done[[1]] <- kv_write(stopped, config, dry_run)

  located <- jobs[jobs$status == "located", ]
  batches <- split(
    seq_len(nrow(located)), ceiling(seq_len(nrow(located)) / workers)
  )
  for (b in seq_along(batches)) {
    cli::cli_rule("Batch {b} of {length(batches)}")
    batch <- located[batches[[b]], ]
    batch <- batch |>
      kv_fetch(config) |>
      kv_process(config, workers = workers) |>
      kv_collate(config) |>
      kv_report(config) |>
      kv_write(config, dry_run)
    if (cleanup) batch <- kv_cleanup(batch, config)
    done[[length(done) + 1]] <- batch
  }

  jobs <- dplyr::bind_rows(done)
  jobs <- jobs[order(match(paste(jobs$record_id, jobs$timepoint), keys)), ]
  log <- write_run_log(jobs, config, started)
  cli::cli_rule("Done")
  report_jobs(jobs, "Run", rows = integer())
  cli::cli_inform(c("i" = cli_escape(paste(
    "Details of each recording are in", log
  ))))
  invisible(jobs)
}

#' Delete the local copies of recordings
#'
#' Step 7 of the workflow. Deletes each recording's folder in the work folder:
#' the raw files copied from OneDrive, GGIR's output and the files made for
#' REDCap. GGIR's logs are kept.
#'
#' @param jobs A job table from any step.
#' @param config A [kv_config()] object.
#'
#' @return The job table, unchanged.
#' @export
kv_cleanup <- function(jobs, config = kv_config()) {
  if (!"job_dir" %in% names(jobs)) {
    return(jobs)
  }
  folders <- jobs$job_dir[!is.na(jobs$job_dir)]
  unlink(folders, recursive = TRUE)
  invisible(jobs)
}

# Saves one row per recording, without names, and returns the file's path.
write_run_log <- function(jobs, config, started) {
  log_dir <- file.path(config$work_dir, "logs")
  fs::dir_create(log_dir)
  path <- file.path(
    log_dir, paste0("run_", format(started, "%Y-%m-%d_%H%M%S"), ".csv")
  )
  log <- tibble::tibble(
    record_id = jobs$record_id,
    timepoint = jobs$timepoint,
    reason = jobs$reason,
    status = jobs$status,
    redcap_status = jobs$redcap_status,
    message = jobs$message,
    notes = vapply(jobs$notes, paste, character(1), collapse = " | ")
  )
  utils::write.csv(log, path, row.names = FALSE, na = "")
  path
}
