# The job table has one row per recording (a participant at a timepoint) and is
# passed from step to step. Each step works on the rows with the status it
# expects and leaves the others alone, so one problem recording never stops a
# run. `message` explains the current status, and `notes` collects things
# that don't stop processing. `check_notes` is TRUE once a note is worth a
# person's attention, which is recorded as "Complete - check notes".
#
# Statuses:
#   pending       selected by kv_pending()
#   located       raw files found by kv_locate()
#   ready         raw files copied and checked by kv_fetch()
#   processed     GGIR ran by kv_process()
#   collated      results merged and REDCap values prepared by kv_collate()
#   reported      participant report created by kv_report()
#   written       results and files written to REDCap by kv_write()
#   no_folder     no folder for the participant or timepoint
#   no_files      no accelerometer folder, or no .bin files in it
#   misnamed      a folder needs renaming on OneDrive before it can be used
#   needs_review  the files need a person to decide what to use
#   invalid_file  a file is unreadable, incomplete, too short or corrupt
#   copy_failed   copying from OneDrive failed; it will be retried next run
#   ggir_failed   GGIR stopped with an error or without writing its results
#   ggir_timeout  GGIR ran out of time; it will be retried
#   no_valid_days GGIR found no day or night with enough wear to use
#   collate_failed GGIR's results couldn't be merged; it will be retried
#   report_failed the report couldn't be created; it will be retried
#   write_failed  writing to REDCap failed part-way; it will be retried

job_statuses <- c(
  "pending", "located", "ready", "processed", "collated", "reported",
  "written", "no_folder", "no_files", "misnamed", "needs_review",
  "invalid_file", "copy_failed", "ggir_failed", "ggir_timeout",
  "no_valid_days", "collate_failed", "report_failed", "write_failed"
)

add_note <- function(jobs, i, note, check = TRUE) {
  jobs$notes[[i]] <- c(jobs$notes[[i]], note)
  jobs$check_notes[[i]] <- jobs$check_notes[[i]] || check
  jobs
}

job_name <- function(jobs, i) {
  paste0(jobs$record_id[[i]], "_", jobs$timepoint[[i]])
}

set_status <- function(jobs, i, status, message = NA_character_) {
  jobs$status[[i]] <- status
  jobs$message[[i]] <- message
  jobs
}

# Prints how many recordings are in each status after a step, and lists the
# recordings in `rows` (those the step worked on) that need someone to act.
# Missing files are only counted; they are listed in the returned job table.
report_jobs <- function(jobs, step, rows = seq_len(nrow(jobs))) {
  if (nrow(jobs) == 0) {
    cli::cli_inform(c("v" = "{step}: no recordings."))
    return(invisible(jobs))
  }
  counts <- table(factor(jobs$status, levels = job_statuses))
  counts <- counts[counts > 0]
  # Only fixed words and counts, so safe to pass to cli without escaping
  summary <- paste(counts, gsub("_", " ", names(counts)), collapse = ", ")
  cli::cli_inform(c("v" = paste0(step, ": ", summary, ".")))

  actionable <- c(
    "misnamed", "needs_review", "invalid_file", "copy_failed", "ggir_failed",
    "ggir_timeout", "collate_failed", "report_failed", "write_failed"
  )
  problems <- jobs[intersect(rows, which(jobs$status %in% actionable)), ]
  if (nrow(problems) > 0) {
    lines <- cli_escape(paste0(
      problems$record_id, " ", problems$timepoint, " (",
      gsub("_", " ", problems$status), "): ", problems$message
    ))
    cli::cli_bullets(rlang::set_names(lines, rep("*", length(lines))))
  }
  invisible(jobs)
}

# Escapes text so cli prints it literally rather than interpolating braces.
cli_escape <- function(x) {
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}
