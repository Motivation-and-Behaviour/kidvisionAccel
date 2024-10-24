#' Generate a report for the accelerometer data
#'
#' @param report_path The path to the report file.
#' @param participant_id The participant ID
#' @param timepoint The time point to upload to. Must be one of "baseline",
#' "12m", or "24m"
#' @param redcap_uri REDCap endpoint. Defaults to the value of the environment
#' variable `REDCAP_URI`
#' @param redcap_key REDCap API key. Defaults to the value of the environment
#' variable `REDCAP_KEY`
#'
#' @return The output of [redcapAPI::importRecords()].
#' @export
upload_report <- function(
    report_path, participant_id, timepoint,
    redcap_uri = Sys.getenv("REDCAP_URI"),
    redcap_key = Sys.getenv("REDCAP_KEY")) {
  rlang::arg_match(arg = timepoint, values = c("baseline", "12m", "24m"))
  stopifnot(
    "`redcap_uri` must be a non-empty string" =
      (redcap_uri != "")
  )
  stopifnot(
    "`redcap_key` must be a non-empty string" =
      (redcap_key != "")
  )
  stopifnot(
    "`participant_id` must be a non-empty string" =
      (participant_id != "")
  )

  timepoint_map <- c(
    "baseline" = "baseline_visit_2_arm_1",
    "12m" = "12_month_visit_arm_1",
    "24m" = "24_month_visit_arm_1"
  )

  rcon <- redcapAPI::redcapConnection(
    url = redcap_uri,
    token = redcap_key
  )

  redcapAPI::importFiles(
    rcon,
    report_path,
    participant_id, "reports_accel", timepoint_map[[timepoint]]
  )

  redcapAPI::importRecords(
    rcon,
    redcapAPI::castForImport(data.frame(
      record_id = participant_id,
      redcap_event_name = timepoint_map[[timepoint]],
      participant_reports_complete = 2
    ), rcon)
  )
}
