#' Find recordings that need processing
#'
#' Step 1 of the workflow. Reads REDCap to find the accelerometer recordings
#' (a participant at a timepoint) that still need processing.
#'
#' A recording is only considered once its visit 2 has happened, i.e. the
#' visit date (`sop2_date`) is set. It is never processed if the participant
#' did not consent to wearing the accelerometer, or withdrew or dropped out
#' and asked for their data to be removed.
#'
#' Which of the remaining recordings are selected depends on their processing
#' status (`cac_proc_status`) and `reprocess`:
#' * `"none"`: recordings that have never been processed, were interrupted
#'   part-way, or failed. Recordings where no file was found are selected
#'   again once a `.bin` file appears in their folder. Recordings that need
#'   manual review, had no valid days, or can't be processed are only
#'   selected when named in `ids`.
#' * `"outdated"`: as for `"none"`, plus completed recordings processed with a
#'   different GGIR version or different GGIR settings. This includes
#'   recordings processed before the status fields existed.
#' * `"all"`: every recording.
#'
#' @param config A [kv_config()] object.
#' @param reprocess Which already-processed recordings to select; see
#'   Details.
#' @param ids Optional record IDs to restrict the selection to.
#' @param timepoints Optional timepoints to restrict the selection to: any of
#'   `"baseline"`, `"12m"` and `"24m"`.
#'
#' @return A job table: a tibble with one row per recording to process, which
#'   the later steps take and return. `reason` gives why each recording was
#'   selected, and `status` is `"pending"`.
#' @export
kv_pending <- function(
  config = kv_config(),
  reprocess = c("none", "outdated", "all"),
  ids = NULL,
  timepoints = NULL
) {
  reprocess <- rlang::arg_match(reprocess)
  if (!is.null(timepoints)) {
    timepoints <- rlang::arg_match(
      timepoints, timepoint_map$timepoint, multiple = TRUE
    )
  }

  rcon <- rc_connect(config)
  records <- rc_export(
    rcon,
    fields = pending_fields,
    events = c(
      enrolment_event, timepoint_map$visit1_event, timepoint_map$visit2_event
    )
  )
  recordings <- recording_states(records)

  # A recording with no file found is retried once a file appears. Only
  # folder listings are read, so nothing is downloaded.
  recheck <- recordings$state == "no file"
  recordings$has_new_file <- FALSE
  recordings$has_new_file[recheck] <- purrr::map2_lgl(
    recordings$record_id[recheck],
    recordings$timepoint[recheck],
    function(id, tp) {
      !recording_files(config$source_dir, id, tp)$status %in%
        c("no_folder", "no_files")
    }
  )

  selected <- select_recordings(recordings, reprocess, ids, timepoints)
  report_selection(recordings, selected)
  new_job_table(recordings[selected, ])
}

# One row per participant and timepoint (visit 2 event), with what is known
# about the recording from REDCap and its `state`.
recording_states <- function(records) {
  value <- function(x) {
    x <- if (is.null(x)) rep(NA_character_, nrow(records)) else x
    x <- trimws(as.character(x))
    x[x == ""] <- NA_character_
    x
  }
  event <- value(records$redcap_event_name)

  # Consent and data-removal requests apply to every timepoint
  participant <- tibble::tibble(
    record_id = value(records$record_id),
    no_consent = value(records$consent_geneactiv) %in% "0",
    remove_data = value(records$withdrawn_existingdata) %in% "2" |
      value(records$dropout_existingdata) %in% "2"
  )
  participant <- dplyr::summarise(
    participant,
    no_consent = any(.data$no_consent),
    remove_data = any(.data$remove_data),
    .by = "record_id"
  )

  visit1 <- tibble::tibble(
    record_id = value(records$record_id),
    timepoint = timepoint_map$timepoint[
      match(event, timepoint_map$visit1_event)
    ],
    visit1_date = as.Date(value(records$sop_date))
  )
  visit1 <- visit1[!is.na(visit1$timepoint), ]

  files_present <- Reduce(
    `&`,
    lapply(file_fields, function(f) !is.na(value(records[[f]])))
  )
  visit2 <- tibble::tibble(
    record_id = value(records$record_id),
    timepoint = timepoint_map$timepoint[
      match(event, timepoint_map$visit2_event)
    ],
    event = event,
    visit2_date = as.Date(value(records$sop2_date)),
    accel_downloaded = value(records$sop2_postchecklist___1) %in% "1",
    device_num = value(records$cac_gen_num),
    files_present = files_present,
    proc_status = suppressWarnings(
      as.integer(value(records$cac_proc_status))
    ),
    proc_ggir = value(records$cac_proc_ggir),
    proc_params = value(records$cac_proc_params)
  )
  visit2 <- visit2[!is.na(visit2$timepoint), ]

  recordings <- visit2 |>
    dplyr::left_join(visit1, by = c("record_id", "timepoint")) |>
    dplyr::left_join(participant, by = "record_id") |>
    dplyr::mutate(
      no_consent = .data$no_consent %in% TRUE,
      remove_data = .data$remove_data %in% TRUE
    )

  recordings$state <- dplyr::case_when(
    recordings$no_consent ~ "no consent",
    recordings$remove_data ~ "data removal requested",
    is.na(recordings$visit2_date) ~ "not yet visited",
    is.na(recordings$proc_status) & recordings$files_present ~ "complete",
    is.na(recordings$proc_status) ~ "new",
    recordings$proc_status == 0 ~ "in progress",
    recordings$proc_status %in% c(1, 2) ~ "complete",
    recordings$proc_status == 3 ~ "no file",
    recordings$proc_status == 4 ~ "no valid days",
    recordings$proc_status == 5 ~ "failed",
    recordings$proc_status == 6 ~ "needs review",
    recordings$proc_status == 7 ~ "cannot be processed",
    .default = "unknown status"
  )
  recordings$outdated <- recordings$state == "complete" & (
    !same_version(recordings$proc_ggir, pinned_versions[["GGIR"]]) |
      !recordings$proc_params %in% ggir_params_hash()
  )

  order <- order(
    suppressWarnings(as.numeric(recordings$record_id)),
    recordings$record_id,
    match(recordings$timepoint, timepoint_map$timepoint)
  )
  recordings[order, ]
}

# Whether each version in `x` is `version`, however it's written: R reports
# GGIR 3.3-9 as "3.3.9". Missing or unreadable versions are not.
same_version <- function(x, version) {
  parsed <- suppressWarnings(package_version(x, strict = FALSE))
  (parsed == package_version(version)) %in% TRUE
}

select_recordings <- function(recordings, reprocess, ids, timepoints) {
  state <- recordings$state
  selected <- state %in% c("new", "in progress", "failed") |
    (state == "no file" & recordings$has_new_file)
  if (!is.null(ids)) {
    selected <- selected |
      state %in% c("no valid days", "needs review", "cannot be processed")
  }
  if (reprocess == "outdated") {
    selected <- selected | recordings$outdated
  }
  if (reprocess == "all") {
    selected <- !state %in%
      c("no consent", "data removal requested", "not yet visited")
  }
  if (!is.null(ids)) {
    selected <- selected & recordings$record_id %in% as.character(ids)
  }
  if (!is.null(timepoints)) {
    selected <- selected & recordings$timepoint %in% timepoints
  }
  selected
}

report_selection <- function(recordings, selected) {
  describe <- function(states) {
    counts <- sort(table(states), decreasing = TRUE)
    paste(counts, names(counts), collapse = ", ")
  }
  n <- sum(selected)
  bullets <- c("v" = "{n} recording{?s} to process.")
  # Only fixed words and counts, so safe to pass to cli without escaping
  if (n > 0) {
    reasons <- describe(selected_reason(recordings[selected, ]))
    bullets <- c(bullets, " " = paste0("Selected: ", reasons, "."))
  }
  if (any(!selected)) {
    others <- describe(recordings$state[!selected])
    bullets <- c(bullets, "i" = paste0("Not selected: ", others, "."))
  }
  cli::cli_inform(bullets)
}

selected_reason <- function(recordings) {
  ifelse(
    recordings$state == "complete",
    ifelse(recordings$outdated, "outdated", "reprocess"),
    recordings$state
  )
}

new_job_table <- function(recordings) {
  tibble::tibble(
    record_id = recordings$record_id,
    timepoint = recordings$timepoint,
    event = recordings$event,
    reason = selected_reason(recordings),
    visit1_date = recordings$visit1_date,
    visit2_date = recordings$visit2_date,
    accel_downloaded = recordings$accel_downloaded,
    device_num = recordings$device_num,
    status = rep("pending", nrow(recordings)),
    message = rep(NA_character_, nrow(recordings)),
    notes = rep(list(character()), nrow(recordings)),
    check_notes = rep(FALSE, nrow(recordings))
  )
}
