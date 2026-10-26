#' GGIR settings used for every KidVision recording
#'
#' The single source of truth for how accelerometer files are processed. The
#' settings target primary-school children wearing a GENEActiv on the
#' non-dominant wrist. Per-recording arguments (`datadir`, `outputdir`,
#' `studyname` and `study_dates_file`) are added when GGIR is run.
#'
#' Key choices:
#' * Intensity thresholds for GENEActiv on the non-dominant wrist in children
#'   aged 7-11: light 56.3 mg (Hildebrand et al. 2017,
#'   \doi{10.1111/sms.12795}), moderate 191.6 mg and vigorous 695.8 mg
#'   (Hildebrand et al. 2014, \doi{10.1249/MSS.0000000000000289}).
#' * Sleep from the HDCZA guider with the van Hees 2015 sustained-inactivity
#'   algorithm, which performed best against polysomnography in school-aged
#'   children (Meredith-Jones et al. 2024, \doi{10.1186/s12966-024-01590-x}).
#' * A valid day needs at least 16 hours of wear. Passing the criterion twice
#'   makes GGIR apply it in part 5 as well as part 2, so both parts agree on
#'   which days are valid.
#' * The timezone is set explicitly, because the devcontainer runs in UTC and
#'   GGIR would otherwise split days at UTC midnight.
#'
#' @param tz Olson name of the timezone the data were collected in.
#'
#' @return `ggir_params()` returns a named list of arguments for
#'   [GGIR::GGIR()]. `ggir_params_hash()` returns a hash of that list, which is
#'   stored with each processed recording so that recordings processed with
#'   different settings can be found. It is the MD5 hash of the settings as
#'   JSON, so it only changes when the settings do, not with the R or package
#'   versions.
#' @export
#' @examples
#' str(ggir_params())
ggir_params <- function(tz = "Australia/Sydney") {
  list(
    # General ------------------------------------------------------------------
    mode = 1:6,
    do.report = c(2, 4, 5, 6),
    do.parallel = FALSE, # Recordings are run in parallel instead
    overwrite = TRUE, # Each recording is processed in a new, empty folder
    desiredtz = tz,
    configtz = tz,
    idloc = 2, # ID is the filename up to the first "_", i.e. the record ID
    maxRecordingInterval = 168, # Append files from a device swap mid-week
    # Part 1: acceleration metric and non-wear --------------------------------
    acc.metric = "ENMO",
    do.enmo = TRUE,
    windowsizes = c(5, 900, 3600),
    nonwear_approach = "2023",
    nonwear_range_threshold = 150, # GGIR 3.1-3 and 3.1-4 briefly used 50
    # Part 2: data quality and day-level summaries -----------------------------
    data_masking_strategy = 2, # Midnight-to-midnight days
    maxdur = 0,
    do.imp = TRUE,
    includedaycrit = c(16, 16), # Second value applies the rule in part 5
    qwindow = c(0, 24),
    iglevels = c(seq(0, 4000, by = 25), 8000),
    MX.ig.min.dur = 10,
    # Acceleration above which the most active 720, 480, ..., 2 minutes of the
    # day are spent (M720 ... M2), for comparison with Fairclough et al. 2023
    qlevels = (1440 - c(720, 480, 240, 120, 60, 30, 15, 10, 5, 2)) / 1440,
    winhr = c(5, 10), # L5 and M10
    ilevels = seq(0, 4000, by = 50),
    # Parts 3 and 4: sleep ----------------------------------------------------
    sensor.location = "wrist",
    HASPT.algo = "HDCZA",
    HASIB.algo = "vanHees2015",
    includenightcrit = 16,
    do.visual = FALSE,
    # Part 5: time use by intensity -----------------------------------------
    threshold.lig = 56.3, # Hildebrand 2017
    threshold.mod = 191.6, # Hildebrand 2014
    threshold.vig = 695.8, # Hildebrand 2014
    timewindow = c("MM", "WW"),
    includedaycrit.part5 = 2 / 3,
    # Part 6: circadian rhythm ---------------------------------------------
    part6CR = TRUE,
    # Output -----------------------------------------------------------------
    visualreport = FALSE,
    epochvalues2csv = FALSE
  )
}

#' @rdname ggir_params
#' @export
ggir_params_hash <- function(tz = "Australia/Sydney") {
  # tools::md5sum() only hashes files before R 4.5
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  writeLines(jsonlite::toJSON(ggir_params(tz), digits = NA), path)
  unname(tools::md5sum(path))
}

# Names in `params` that the installed GGIR does not recognise. GGIR errors on
# unknown names, so this catches settings removed or renamed in a new version.
ggir_unknown_params <- function(params) {
  known <- c(
    names(formals(GGIR::GGIR)),
    unlist(lapply(GGIR::load_params(), names), use.names = FALSE)
  )
  setdiff(names(params), known)
}
