#' Locate the raw accelerometer files for each recording
#'
#' Step 2 of the workflow, part 1. Finds each recording's GENEActiv `.bin`
#' files on OneDrive using only folder listings and file names, so nothing is
#' downloaded.
#'
#' Files are expected in `<source_dir>/<record_id>/<timepoint>/Accelerometer/`,
#' where the timepoint folder is `Baseline`, `Time_1` or `Time_2`. Files in
#' subfolders of `Accelerometer` are included, except in a subfolder named
#' `Excluded`, which is the place to put files that should not be used.
#' Folders with other names (e.g. `Time 1` or `Accelerometers`) are not
#' guessed at: the recording is marked `"misnamed"` with the rename needed.
#'
#' GENEActiv names files `<subject>__<serial>_<download time>.bin`, which
#' gives these checks without opening the files:
#' * Copies of the same file (same device and download time) are ignored.
#' * A file whose subject code doesn't match the participant needs review.
#' * A file downloaded on or before the visit 1 date needs review, as it can't
#'   be from this timepoint's recording and may be in the wrong folder.
#'
#' Checks that need the recording's own start and end times, including how
#' several files for a recording are combined, are made by [kv_fetch()] once
#' the files are copied.
#'
#' @param jobs A job table from [kv_pending()].
#' @param config A [kv_config()] object.
#'
#' @return The job table, with the accelerometer folder in `folder` and a
#'   tibble of its files in `files`. Recordings whose files were found have
#'   status `"located"`; the others have status `"no_folder"`, `"no_files"`,
#'   `"misnamed"`, `"needs_review"` or `"invalid_file"`, with the reason in
#'   `message`.
#' @export
kv_locate <- function(jobs, config = kv_config()) {
  check_source_mounted(config$source_dir)
  if (!"folder" %in% names(jobs)) jobs$folder <- NA_character_
  if (!"files" %in% names(jobs)) jobs$files <- rep(list(NULL), nrow(jobs))

  rows <- which(jobs$status == "pending")
  source_folders <- folder_names(config$source_dir)
  no_participant_folder <- 0
  for (i in rows) {
    found <- recording_files(
      config$source_dir, jobs$record_id[[i]], jobs$timepoint[[i]],
      tz = config$tz, source_folders = source_folders
    )
    no_participant_folder <- no_participant_folder +
      isTRUE(found$no_participant_folder)
    jobs$folder[[i]] <- found$folder
    jobs$files[i] <- list(found$files)
    message <- found$message
    # Missing files are more likely a problem when the visit 2 checklist
    # says the data were downloaded
    missing <- found$status %in% c("no_folder", "no_files")
    if (missing && isTRUE(jobs$accel_downloaded[[i]])) {
      message <- paste(
        message,
        "The visit 2 checklist says the accelerometer data were downloaded."
      )
    }
    jobs <- set_status(jobs, i, found$status, message)
    for (note in found$notes) jobs <- add_note(jobs, i, note)
    if (found$status == "located") {
      jobs <- check_download_date(jobs, i)
    }
  }

  jobs <- check_shared_files(jobs, rows)

  # Most recordings missing their participant folder points to a problem with
  # the OneDrive folder or mount, not hundreds of missing recordings.
  if (length(rows) >= 10 && no_participant_folder > length(rows) / 2) {
    cli::cli_abort(c(
      "{no_participant_folder} of {length(rows)} recordings have no \\
      participant folder in {.path {config$source_dir}}.",
      "i" = "Check that OneDrive has finished syncing and the folder is \\
      mounted correctly."
    ))
  }

  report_jobs(jobs, "Locate", rows)
}

check_source_mounted <- function(source_dir) {
  mounted <- nzchar(source_dir) && fs::dir_exists(source_dir) &&
    length(participant_folders(source_dir)) > 0
  if (!mounted) {
    cli::cli_abort(c(
      "No participant folders found in {.path {source_dir}}.",
      "i" = "Is OneDrive running, and is the folder mounted? \\
      Run {.fn kv_check} for details."
    ))
  }
}

participant_folders <- function(source_dir) {
  fs::dir_ls(source_dir, type = "directory", regexp = "/[0-9]+$")
}

# Finds the files for one recording. Returns a list with `status`, `message`,
# `folder` (the accelerometer folder), `files` and `notes`. `source_folders`
# is the names of the folders in `source_dir`, so callers can list it once
# for many recordings.
recording_files <- function(source_dir, record_id, timepoint, tz = "",
                            source_folders = folder_names(source_dir)) {
  result <- function(status, message = NA_character_, folder = NA_character_,
                     files = NULL, notes = character(), ...) {
    list(
      status = status, message = message, folder = folder, files = files,
      notes = notes, ...
    )
  }
  # A folder that wasn't found is misnamed if there are variants of it
  not_found <- function(found, name, status, message, ...) {
    if (length(found$variants) > 0) {
      return(result("misnamed", rename_message(found, name, source_dir)))
    }
    result(status, message, ...)
  }

  participant <- find_folder(
    source_dir, record_id, paste0("(^|\\D)", record_id, "(\\D|$)"),
    source_folders
  )
  if (is.na(participant$path)) {
    return(not_found(
      participant, record_id, "no_folder", "No participant folder.",
      no_participant_folder = TRUE
    ))
  }

  tp <- timepoint_map[timepoint_map$timepoint == timepoint, ]
  timepoint_dir <- find_folder(participant$path, tp$folder, tp$misnamed_pattern)
  if (is.na(timepoint_dir$path)) {
    return(not_found(
      timepoint_dir, tp$folder, "no_folder",
      glue::glue("No {tp$folder} folder.")
    ))
  }

  accel_dir <- find_folder(timepoint_dir$path, accel_folder, "^accel")
  if (is.na(accel_dir$path)) {
    return(not_found(
      accel_dir, accel_folder, "no_files",
      glue::glue("No {accel_folder} folder.")
    ))
  }

  files <- bin_files(accel_dir$path, tz)
  if (nrow(files) == 0) {
    return(result("no_files", "No .bin files.", folder = accel_dir$path))
  }
  checked <- check_files(files, record_id)
  result(
    checked$status, checked$message,
    folder = accel_dir$path, files = checked$files, notes = checked$notes
  )
}

# Looks for a folder called `name` (ignoring case) in `parent`, whose folders
# are `folders`. If it isn't there, `variants` lists folders matching
# `pattern` that look like misnamed versions of it.
find_folder <- function(parent, name, pattern,
                        folders = folder_names(parent)) {
  exact <- folders[tolower(folders) == tolower(name)]
  if (length(exact) == 1) {
    return(list(path = file.path(parent, exact), variants = character()))
  }
  variants <- folders[grepl(pattern, folders, ignore.case = TRUE, perl = TRUE)]
  list(path = NA_character_, parent = parent, variants = variants)
}

# The names of the folders in `parent`, or none if it doesn't exist.
folder_names <- function(parent) {
  if (fs::dir_exists(parent)) {
    basename(fs::dir_ls(parent, type = "directory"))
  } else {
    character()
  }
}

rename_message <- function(found, name, source_dir) {
  where <- fs::path_rel(found$parent, source_dir)
  where <- if (where == ".") "the source folder" else where
  glue::glue(
    "Found {paste0('\"', found$variants, '\"', collapse = ' and ')} in ",
    "{where}; rename to \"{name}\" if this holds the recording."
  )
}

# Lists the .bin files under an accelerometer folder, skipping any `Excluded`
# subfolder, with details parsed from GENEActiv's file names.
bin_files <- function(accel_dir, tz) {
  paths <- fs::dir_ls(
    accel_dir,
    recurse = TRUE, type = "file", regexp = "[.]bin$", ignore.case = TRUE
  )
  relative <- fs::path_rel(paths, accel_dir)
  in_excluded <- vapply(fs::path_split(relative), function(parts) {
    folders <- parts[-length(parts)]
    any(tolower(folders) == tolower(excluded_folder))
  }, logical(1))
  paths <- sort(as.character(paths[!in_excluded]))
  details <- parse_bin_filename(basename(paths), tz)
  tibble::tibble(
    path = paths,
    size = as.numeric(fs::file_size(paths)),
    subject = details$subject,
    serial = details$serial,
    downloaded = details$downloaded
  )
}

# GENEActiv names files "<subject>__<serial>_<YYYY-MM-DD HH-MM-SS>.bin", where
# the time is when the data were downloaded from the device, in the local time
# of the computer used.
parse_bin_filename <- function(filename, tz = "") {
  pattern <- paste0(
    "^(.*?)__([0-9]+)_",
    "([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}-[0-9]{2}-[0-9]{2})[.]bin$"
  )
  parts <- regmatches(
    filename, regexec(pattern, filename, ignore.case = TRUE, perl = TRUE)
  )
  part <- function(n) {
    vapply(parts, function(p) if (length(p) > 0) p[[n]] else NA_character_, "")
  }
  tibble::tibble(
    subject = part(2),
    serial = part(3),
    downloaded = as.POSIXct(part(4), format = "%Y-%m-%d %H-%M-%S", tz = tz)
  )
}

# Applies the checks that need only file names and sizes.
check_files <- function(files, record_id) {
  notes <- character()
  outcome <- function(status, message = NA_character_) {
    list(status = status, message = message, files = files, notes = notes)
  }

  empty <- files$size == 0
  if (all(empty)) {
    return(outcome("invalid_file", "The .bin files are empty."))
  }
  if (any(empty)) {
    notes <- c(notes, glue::glue(
      "Ignored empty file {basename(files$path[empty])}."
    ))
    files <- files[!empty, ]
  }

  duplicate <- !is.na(files$serial) & !is.na(files$downloaded) &
    duplicated(files[c("serial", "downloaded")])
  if (any(duplicate)) {
    notes <- c(notes, glue::glue(
      "Ignored {basename(files$path[duplicate])}, a copy of another file."
    ))
    files <- files[!duplicate, ]
  }

  subject_id <- sub("[^0-9].*$", "", files$subject)
  other <- !is.na(files$subject) & nzchar(files$subject) &
    subject_id != record_id
  if (any(other)) {
    return(outcome("needs_review", glue::glue(
      "{basename(files$path[other])} is labelled as participant ",
      "\"{files$subject[other]}\". Move it to the right participant, or to ",
      "an \"{excluded_folder}\" subfolder."
    )))
  }

  outcome("located")
}

# The same file (same device and download time) in two recordings means at
# least one copy is in the wrong folder.
check_shared_files <- function(jobs, rows) {
  has_files <- rows[!vapply(jobs$files[rows], is.null, logical(1))]
  if (length(has_files) < 2) {
    return(jobs)
  }
  files <- dplyr::bind_rows(jobs$files[has_files], .id = "row")
  files$row <- has_files[as.integer(files$row)]
  files <- files[!is.na(files$serial) & !is.na(files$downloaded), ]
  key <- paste(files$serial, format(files$downloaded))
  recordings_per_key <- tapply(files$row, key, function(r) length(unique(r)))
  shared <- key %in% names(recordings_per_key)[recordings_per_key > 1]

  for (i in unique(files$row[shared])) {
    mine <- files[files$row == i & shared, ]
    others <- files[files$row != i & key %in% key[files$row == i & shared], ]
    elsewhere <- unique(paste(
      jobs$record_id[others$row], jobs$timepoint[others$row]
    ))
    message <- glue::glue(
      "{paste(basename(mine$path), collapse = ', ')} is the same file ",
      "(device and download time) as one in ",
      "{paste(elsewhere, collapse = ', ')}. Remove the copy from the wrong ",
      "folder."
    )
    if (jobs$status[[i]] == "needs_review") {
      message <- paste(jobs$message[[i]], message)
    }
    jobs <- set_status(jobs, i, "needs_review", message)
  }
  jobs
}

# Devices are downloaded after they are collected at visit 2, so a file
# downloaded on or before the visit 1 date can't be this timepoint's recording.
check_download_date <- function(jobs, i) {
  files <- jobs$files[[i]]
  downloaded <- local_date(files$downloaded)
  early <- (downloaded <= jobs$visit1_date[[i]]) %in% TRUE
  if (!any(early)) {
    return(jobs)
  }
  set_status(jobs, i, "needs_review", glue::glue(
    "{paste(basename(files$path[early]), collapse = ', ')} was downloaded on ",
    "{paste(downloaded[early], collapse = ', ')}, but visit 1 was ",
    "{jobs$visit1_date[[i]]}. Is it in the right timepoint folder, and is ",
    "the visit 1 date right?"
  ))
}

#' Check the OneDrive folder for naming problems
#'
#' Lists the folders the pipeline can't use as they are, so they can be fixed
#' on OneDrive: participant and timepoint folders with non-standard names,
#' misnamed accelerometer folders, and accelerometer folders whose files need
#' review (see [kv_locate()]). Only folder listings and file names are read,
#' so nothing is downloaded. Unlike [kv_locate()], this doesn't use REDCap, so
#' it can't check files against visit dates.
#'
#' @param config A [kv_config()] object.
#'
#' @return A tibble with columns `folder` (relative to the source folder),
#'   `problem` and `fix`.
#' @export
kv_audit_folders <- function(config = kv_config()) {
  check_source_mounted(config$source_dir)
  source_dir <- config$source_dir
  participants <- fs::dir_ls(source_dir, type = "directory")
  source_folders <- basename(participants)
  participants <- participants[!source_folders %in% ignored_folders]

  issues <- dplyr::bind_rows(
    folder_issue(character(), character(), character()),
    unname(lapply(
      participants, audit_participant, source_dir, config$tz, source_folders
    ))
  )
  issues$folder <- as.character(fs::path_rel(issues$folder, source_dir))
  cli::cli_inform(c(
    "i" = "{nrow(issues)} folder{?s} with problems in {.path {source_dir}}."
  ))
  issues
}

folder_issue <- function(folder, problem, fix) {
  tibble::tibble(
    folder = as.character(folder),
    problem = as.character(problem),
    fix = as.character(fix)
  )
}

audit_participant <- function(participant, source_dir, tz, source_folders) {
  record_id <- basename(participant)
  if (!grepl("^[0-9]+$", record_id)) {
    return(folder_issue(
      participant, "Not named as a record ID", "Rename to the record ID"
    ))
  }

  audit_timepoint <- function(folder) {
    exact <- tolower(basename(folder)) == tolower(timepoint_map$folder)
    if (!any(exact)) {
      variant <- vapply(
        timepoint_map$misnamed_pattern, grepl, logical(1),
        x = basename(folder), ignore.case = TRUE, perl = TRUE
      )
      if (any(variant)) {
        return(folder_issue(folder, "Misnamed timepoint folder", glue::glue(
          "Rename to \"{timepoint_map$folder[variant][[1]]}\""
        )))
      }
      return(folder_issue(
        folder, "Unrecognised folder", "None needed; it is ignored"
      ))
    }
    found <- recording_files(
      source_dir, record_id, timepoint_map$timepoint[exact], tz = tz,
      source_folders = source_folders
    )
    if (found$status == "misnamed") {
      return(folder_issue(folder, "Misnamed accelerometer folder", glue::glue(
        "Rename to \"{accel_folder}\""
      )))
    }
    if (found$status %in% c("needs_review", "invalid_file")) {
      return(folder_issue(found$folder, found$message, "See problem"))
    }
    NULL
  }

  dplyr::bind_rows(
    unname(lapply(fs::dir_ls(participant, type = "directory"), audit_timepoint))
  )
}

# Folders in the source folder that are not participants.
ignored_folders <- c("0000", "Practice Data")
