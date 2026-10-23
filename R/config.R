# Versions of the packages that determine the processed values. Changing these
# changes the data, so bump them deliberately (with a NEWS.md entry) and then
# reprocess with `kv_run(reprocess = "outdated")`.
pinned_versions <- c(GGIR = "3.3-9", GGIRread = "1.0.11")

enrolment_event <- "enrolment_arm_1"

# Study timepoints. `folder` is the timepoint folder's name on OneDrive
# (matched ignoring case). `misnamed_pattern` catches variants such as "Time 1",
# "Time1" or "Time_1 - Copy", which are reported for renaming rather than
# used. The accelerometer is fitted at visit 1 and collected at visit 2, and the
# accelerometer instruments live on the visit 2 event.
timepoint_map <- tibble::tribble(
  ~timepoint, ~folder, ~misnamed_pattern, ~visit1_event, ~visit2_event,
  "baseline", "Baseline", "^base[ _-]?line",
  "baseline_visit_1_arm_1", "baseline_visit_2_arm_1",
  "12m", "Time_1", "^time[ _-]?1(\\D|$)",
  "12m_visit_1_arm_1", "12m_visit_2_arm_1",
  "24m", "Time_2", "^time[ _-]?2(\\D|$)",
  "24m_visit_1_arm_1", "24m_visit_2_arm_1"
)

# The folder holding the raw files within each timepoint folder.
accel_folder <- "Accelerometer"

# Subfolders of the accelerometer folder with this name are ignored, so files
# that should not be processed can be set aside without deleting them.
excluded_folder <- "Excluded"

#' Configure a KidVision processing run
#'
#' Collects the credentials, paths and options used by every step of the
#' workflow. Each argument defaults to an environment variable, so these can
#' live in `.env` (which the devcontainer loads) rather than in code.
#'
#' @param redcap_uri REDCap API endpoint. Defaults to `REDCAP_URI`, or the ACU
#'   REDCap API if that is unset.
#' @param redcap_key REDCap API token. Defaults to `REDCAP_KEY`.
#' @param source_dir Root of the synced OneDrive folder, containing one folder
#'   per participant. Defaults to `KIDVISION_SOURCE_DIR`.
#' @param work_dir Local folder for copied raw files, GGIR output and logs.
#'   Defaults to `KIDVISION_WORK_DIR`.
#' @param tz Olson name of the timezone the data were collected in. Defaults
#'   to `KIDVISION_TZ`, or `"Australia/Sydney"`.
#' @param workers Number of recordings to process in parallel. Defaults to
#'   `KIDVISION_WORKERS`, or 1.
#' @param min_free_gb Free disk space, in GB, required in `work_dir` before a
#'   run starts.
#'
#' @return A `kv_config` object.
#' @export
#' @examples
#' kv_config(source_dir = "/onedrive", work_dir = tempdir())
kv_config <- function(
  redcap_uri = env_or("REDCAP_URI", "https://redcap.acu.edu.au/api/"),
  redcap_key = env_or("REDCAP_KEY", ""),
  source_dir = env_or("KIDVISION_SOURCE_DIR", ""),
  work_dir = env_or("KIDVISION_WORK_DIR", ""),
  tz = env_or("KIDVISION_TZ", "Australia/Sydney"),
  workers = env_or("KIDVISION_WORKERS", "1"),
  min_free_gb = 20
) {
  workers <- suppressWarnings(as.numeric(workers))
  valid_workers <- length(workers) == 1 && !is.na(workers) &&
    workers >= 1 && workers %% 1 == 0
  if (!valid_workers) {
    cli::cli_abort("{.arg workers} must be a single positive whole number.")
  }
  structure(
    list(
      redcap_uri = redcap_uri,
      redcap_key = redcap_key,
      source_dir = source_dir,
      work_dir = work_dir,
      tz = tz,
      workers = as.integer(workers),
      min_free_gb = min_free_gb
    ),
    class = "kv_config"
  )
}

#' @export
print.kv_config <- function(x, ...) {
  value <- function(v) if (nzchar(v)) v else "<not set>"
  rows <- c(
    "REDCap URI" = value(x$redcap_uri),
    "REDCap key" = if (nzchar(x$redcap_key)) "<set>" else "<not set>",
    "Source folder" = value(x$source_dir),
    "Work folder" = value(x$work_dir),
    "Timezone" = x$tz,
    "Workers" = x$workers
  )
  cat("<kv_config>\n", paste0("  ", names(rows), ": ", rows, "\n"), sep = "")
  invisible(x)
}

# Like Sys.getenv(), but treats a variable set to "" as unset.
env_or <- function(name, default) {
  value <- Sys.getenv(name)
  if (nzchar(value)) value else default
}
