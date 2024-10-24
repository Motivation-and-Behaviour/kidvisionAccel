#' Process an accelerometer file
#'
#' @param file_path The path to the accelerometer file
#' @param participant_id The participant ID
#' @param overwrite Overwrite existing files? Set to `TRUE` to rerun processing.
#'
#' @return Nothing. The function writes files to the `file_path` directory.
#' @export
process_file <- function(file_path, participant_id, overwrite = FALSE) {
  # TODO: Validate file_path

  threshold_lig <- 51.6 # Hurter 2018
  threshold_mod <- 191.6 # Hildebrand 2014
  threshold_vig <- 695.8 # Hildebrand 2014

  accel_file <- list.files(file_path, pattern = ".bin$", full.names = TRUE)


  GGIR::GGIR(
    mode = c(1:5),
    datadir = accel_file,
    outputdir = file_path,
    do.report = c(2, 4, 5),
    overwrite = overwrite,
    do.enmo = TRUE,
    do.hfen = FALSE,
    do.enmoa = FALSE,
    studyname = participant_id,
    # =====================
    # Part 2
    # =====================
    do.imp = TRUE,
    strategy = 2,
    maxdur = 0,
    includedaycrit = 16,
    qwindow = c(0, 6, 12, 18, 24),
    mvpathreshold = threshold_mod,
    excludefirstlast = FALSE,
    includenightcrit = 16,
    MX.ig.min.dur = 10,
    iglevels = 50,
    ilevels = seq(0, 4000, 50),
    epochvalues2csv = FALSE,
    winhr = c(
      16, # Most active 16 hours
      c(60 / 60), # Most active hour
      c(30 / 60), # Most active half hour
      c(15 / 60), # Most active 15 min
      c(10 / 60), # Most active 10 min
      c(5 / 60) # Most active 5 min)
    ),
    qlevels = c(
      960 / 1440, # 2/3rds of the day
      1320 / 1440, # Top 120min
      1380 / 1440, # Top 60min
      1410 / 1440, # Top 30min
      1425 / 1440, # Top 15min
      1435 / 1440 # Top 5min
    ),
    idloc = 2,
    acc.metric = "ENMO",
    # =====================
    # Part 3 + 4
    # =====================
    def.noc.sleep = 1,
    outliers.only = TRUE,
    criterror = 4,
    do.visual = TRUE,
    # =====================
    # Part 5
    # =====================
    threshold.lig = threshold_lig,
    threshold.mod = threshold_mod,
    threshold.vig = threshold_vig,
    boutcriter = 0.8, boutcriter.in = 0.9, boutcriter.lig = 0.8,
    boutcriter.mvpa = 0.8, boutdur.in = c(1, 10, 30), boutdur.lig = c(1, 10),
    boutdur.mvpa = c(1),
    includedaycrit.part5 = 2 / 3,
    # =====================
    # Visual report
    # =====================
    timewindow = c("WW", "MM"),
    visualreport = TRUE
  )
}
