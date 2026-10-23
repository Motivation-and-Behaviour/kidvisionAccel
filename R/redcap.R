# Thin wrappers around redcapAPI. All REDCap access goes through these so
# tests can replace them with testthat::local_mocked_bindings().

rc_connect <- function(config) {
  redcapAPI::redcapConnection(
    url = config$redcap_uri,
    token = config$redcap_key
  )
}

rc_project_info <- function(rcon) rcon$projectInformation()

rc_metadata <- function(rcon) rcon$metadata()

rc_events <- function(rcon) rcon$events()

# Exports records as uncast strings, so values reach us exactly as REDCap
# stores them.
rc_export <- function(rcon, fields, events = NULL, records = NULL) {
  redcapAPI::exportRecordsTyped(
    rcon,
    fields = fields,
    events = events,
    records = records,
    cast = redcapAPI::raw_cast
  )
}
