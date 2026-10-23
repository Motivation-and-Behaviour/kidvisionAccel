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

# Checks values against the project before they are imported. redcapAPI
# replaces values that fail its checks (such as text in a number field) with
# NA and only prints a message, and importing that NA with overwriting would
# erase what is in REDCap. So any value that would be lost is an error.
rc_validate <- function(rcon, data) {
  problems <- character()
  prepared <- withCallingHandlers(
    redcapAPI::importRecords(
      rcon, data,
      overwriteBehavior = "overwrite", returnData = TRUE
    ),
    message = function(m) {
      problems <<- c(problems, trimws(conditionMessage(m)))
      invokeRestart("muffleMessage")
    }
  )
  lost <- vapply(names(data), function(field) {
    !field %in% names(prepared) ||
      any(!is.na(data[[field]]) & is.na(prepared[[field]]))
  }, logical(1))
  if (any(lost)) {
    stop(
      "REDCap would not accept ", paste(names(data)[lost], collapse = ", "),
      ". ", paste(problems[nzchar(problems)], collapse = " "),
      call. = FALSE
    )
  }
  invisible(data)
}

# Imports records, replacing existing values, including with blanks.
rc_import_records <- function(rcon, data) {
  response <- suppressMessages(redcapAPI::importRecords(
    rcon, data,
    overwriteBehavior = "overwrite", returnContent = "count"
  ))
  # REDCap replies with the number of records imported, as text
  count <- suppressWarnings(as.integer(
    regmatches(response, regexpr("[0-9]+\\s*$", response))
  ))
  if (!identical(count, nrow(data))) {
    stop("REDCap imported ", count, " of ", nrow(data), " records.",
      call. = FALSE
    )
  }
  invisible(count)
}

rc_import_file <- function(rcon, file, record, field, event) {
  redcapAPI::importFiles(
    rcon,
    file = file, record = record, field = field, event = event,
    overwrite = TRUE
  )
}
