#' Check that everything needed for a run is in place
#'
#' Step 0 of the workflow. Checks:
#' * the pinned GGIR and GGIRread versions are installed;
#' * the installed GGIR recognises every setting in [ggir_params()];
#' * the timezone is valid;
#' * the source folder is reachable and contains participant folders, so an
#'   unmounted or unsynced OneDrive folder is caught before anything is marked
#'   as missing;
#' * the work folder is writable and has enough free space;
#' * Quarto with Typst is available to render reports;
#' * REDCap is reachable, and the fields, events and identifier access the
#'   pipeline needs are all there.
#'
#' @param config A [kv_config()] object.
#' @param dry_run Set to `TRUE` if the run will not write to REDCap. Missing
#'   processing-status fields are then a warning rather than a failure.
#' @param error If `TRUE`, abort when any check fails.
#'
#' @return Invisibly, a tibble with one row per check and columns `check`,
#'   `status` (`"pass"`, `"warn"` or `"fail"`) and `message`.
#' @export
kv_check <- function(config = kv_config(), dry_run = FALSE, error = TRUE) {
  results <- dplyr::bind_rows(
    safely_check("Package versions", check_versions()),
    safely_check("GGIR settings", check_ggir_params(config$tz)),
    safely_check("Timezone", check_timezone(config$tz)),
    safely_check("Source folder", check_source_dir(config$source_dir)),
    safely_check(
      "Work folder", check_work_dir(config$work_dir, config$min_free_gb)
    ),
    safely_check("Quarto", check_quarto()),
    safely_check("REDCap connection", check_redcap(config, dry_run))
  )
  report_checks(results)

  failed <- results$check[results$status == "fail"]
  if (error && length(failed) > 0) {
    cli::cli_abort(c(
      "{length(failed)} check{?s} failed: {failed}.",
      "i" = "Fix the failed checks before running the pipeline."
    ))
  }
  invisible(results)
}

check_result <- function(check, status, message) {
  tibble::tibble(check = check, status = status, message = message)
}

# Evaluates a check, turning any unexpected error into a failed check.
safely_check <- function(check, code) {
  tryCatch(
    code,
    error = function(e) check_result(check, "fail", conditionMessage(e))
  )
}

report_checks <- function(results) {
  alerts <- list(
    pass = cli::cli_alert_success,
    warn = cli::cli_alert_warning,
    fail = cli::cli_alert_danger
  )
  for (i in seq_len(nrow(results))) {
    alerts[[results$status[[i]]]](
      "{results$check[[i]]}: {results$message[[i]]}"
    )
  }
}

installed_version <- function(pkg) {
  tryCatch(
    as.character(utils::packageVersion(pkg)),
    error = function(e) NA_character_
  )
}

check_versions <- function() {
  rows <- purrr::imap(pinned_versions, function(pinned, pkg) {
    check <- paste(pkg, "version")
    install <- glue::glue('Install it with `pak::pak("cran/{pkg}@{pinned}")`.')
    installed <- installed_version(pkg)
    if (is.na(installed)) {
      return(check_result(check, "fail", glue::glue(
        "{pkg} is not installed. {install}"
      )))
    }
    if (package_version(installed) != package_version(pinned)) {
      return(check_result(check, "fail", glue::glue(
        "{pkg} {installed} is installed, but {pinned} is required. {install}"
      )))
    }
    check_result(check, "pass", pinned)
  })
  purrr::list_rbind(unname(rows))
}

check_ggir_params <- function(tz) {
  check <- "GGIR settings"
  params <- ggir_params(tz)
  unknown <- ggir_unknown_params(params)
  if (length(unknown) > 0) {
    return(check_result(check, "fail", glue::glue(
      "The installed GGIR does not recognise {collapse_names(unknown)}."
    )))
  }
  check_result(check, "pass", glue::glue(
    "All {length(params)} settings are recognised by GGIR"
  ))
}

check_timezone <- function(tz) {
  if (!tz %in% OlsonNames()) {
    return(check_result("Timezone", "fail", glue::glue(
      '"{tz}" is not a valid timezone name.'
    )))
  }
  check_result("Timezone", "pass", tz)
}

check_source_dir <- function(path) {
  check <- "Source folder"
  if (!nzchar(path)) {
    return(check_result(check, "fail", paste(
      "Not set. Set KIDVISION_SOURCE_DIR to the synced",
      "'Square Eyes - Community Sample' folder."
    )))
  }
  if (!fs::dir_exists(path)) {
    return(check_result(check, "fail", glue::glue("{path} does not exist.")))
  }
  # Listing the folder does not make OneDrive download anything.
  participants <- fs::dir_ls(path, type = "directory", regexp = "/[0-9]+[^/]*$")
  if (length(participants) == 0) {
    return(check_result(check, "fail", glue::glue(
      "No participant folders in {path}. ",
      "Is OneDrive running, and is the folder mounted?"
    )))
  }
  check_result(check, "pass", glue::glue(
    "{length(participants)} participant folders in {path}"
  ))
}

check_work_dir <- function(path, min_free_gb) {
  check <- "Work folder"
  if (!nzchar(path)) {
    return(check_result(check, "fail", paste(
      "Not set. Set KIDVISION_WORK_DIR to a local folder for temporary files."
    )))
  }
  if (!fs::dir_exists(path)) {
    return(check_result(check, "fail", glue::glue("{path} does not exist.")))
  }
  if (file.access(path, mode = 2) != 0) {
    return(check_result(check, "fail", glue::glue("{path} is not writable.")))
  }
  free_gb <- disk_free_gb(path)
  if (free_gb < min_free_gb) {
    return(check_result(check, "fail", glue::glue(
      "Only {round(free_gb)} GB free in {path}; ",
      "at least {min_free_gb} GB is needed."
    )))
  }
  check_result(check, "pass", glue::glue("{path} ({round(free_gb)} GB free)"))
}

disk_free_gb <- function(path) {
  ps::ps_disk_usage(path)$available / 1e9
}

check_quarto <- function() {
  check <- "Quarto"
  quarto <- Sys.which("quarto")
  if (!nzchar(quarto)) {
    return(check_result(check, "fail", paste(
      "Quarto was not found. It is needed to render participant reports."
    )))
  }
  run <- function(args) {
    out <- suppressWarnings(system2(quarto, args, stdout = TRUE, stderr = TRUE))
    if (!is.null(attr(out, "status"))) NULL else out[[1]]
  }
  version <- run("--version")
  typst <- run(c("typst", "--version"))
  if (is.null(typst)) {
    return(check_result(check, "fail", glue::glue(
      "Quarto {version} does not include Typst. Install Quarto 1.4 or later."
    )))
  }
  check_result(check, "pass", glue::glue("Quarto {version} with {typst}"))
}

check_redcap <- function(config, dry_run) {
  check <- "REDCap connection"
  if (!nzchar(config$redcap_uri) || !nzchar(config$redcap_key)) {
    return(check_result(
      check, "fail", "REDCAP_URI and REDCAP_KEY must both be set."
    ))
  }
  rcon <- rc_connect(config)
  project <- tryCatch(rc_project_info(rcon), error = function(e) e)
  if (inherits(project, "error")) {
    return(check_result(check, "fail", glue::glue(
      "Could not connect to {config$redcap_uri}: {conditionMessage(project)}"
    )))
  }
  production <- if (identical(as.character(project$in_production), "1")) {
    " (in production)"
  } else {
    ""
  }

  dplyr::bind_rows(
    check_result(check, "pass", glue::glue(
      'Project {project$project_id} "{project$project_title}"{production}'
    )),
    safely_check(
      "REDCap fields", check_redcap_fields(rc_metadata(rcon), dry_run)
    ),
    safely_check("REDCap events", check_redcap_events(rc_events(rcon))),
    safely_check("REDCap identifiers", check_redcap_identifiers(rcon))
  )
}

check_redcap_fields <- function(metadata, dry_run) {
  fields <- redcap_fields()
  missing <- setdiff(fields$field, metadata$field_name)
  is_status <- fields$role == "status"
  missing_required <- intersect(missing, fields$field[!is_status])
  missing_status <- intersect(missing, fields$field[is_status])

  required <- if (length(missing_required) > 0) {
    check_result("REDCap fields", "fail", glue::glue(
      "Missing from the project: {collapse_names(missing_required)}."
    ))
  } else {
    check_result("REDCap fields", "pass", glue::glue(
      "All {sum(!is_status)} fields used by the pipeline exist"
    ))
  }

  status <- if (length(missing_status) > 0) {
    check_result(
      "REDCap status fields",
      if (dry_run) "warn" else "fail",
      glue::glue(
        "Missing {collapse_names(missing_status)}. Add them to the ",
        "accelerometer_output instrument before writing to REDCap."
      )
    )
  } else {
    check_result("REDCap status fields", "pass", "All exist")
  }

  dplyr::bind_rows(required, status)
}

check_redcap_events <- function(events) {
  needed <- c(
    enrolment_event, timepoint_map$visit1_event, timepoint_map$visit2_event
  )
  missing <- setdiff(needed, events$unique_event_name)
  if (length(missing) > 0) {
    return(check_result("REDCap events", "fail", glue::glue(
      "Missing from the project: {collapse_names(missing)}."
    )))
  }
  check_result("REDCap events", "pass", glue::glue(
    "All {length(needed)} events exist"
  ))
}

# Reports are personalised with first names, so the API user needs rights to
# export identifiers. With de-identified rights REDCap drops these columns.
check_redcap_identifiers <- function(rcon) {
  check <- "REDCap identifiers"
  ids <- rc_export(rcon, fields = "record_id", events = enrolment_event)
  if (nrow(ids) == 0) {
    return(check_result(check, "warn", "The project has no records yet."))
  }
  names <- rc_export(
    rcon,
    fields = "cid_namef",
    events = enrolment_event,
    records = utils::head(ids$record_id, 50)
  )
  if (!"cid_namef" %in% names(names)) {
    return(check_result(check, "fail", paste(
      "The API token cannot export identifier fields, which reports need.",
      "Its user needs full data export rights."
    )))
  }
  if (all(is.na(names$cid_namef) | names$cid_namef == "")) {
    return(check_result(check, "warn", paste(
      "Child names are blank for the first records checked;",
      "reports may not be personalised."
    )))
  }
  check_result(check, "pass", "Identifier fields can be exported")
}

collapse_names <- function(x, max = 10) {
  if (length(x) > max) {
    x <- c(utils::head(x, max), glue::glue("and {length(x) - max} more"))
  }
  paste(x, collapse = ", ")
}
