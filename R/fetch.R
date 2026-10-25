#' Copy raw files from OneDrive and check them
#'
#' Step 2 of the workflow, part 2. Copies each located recording's `.bin`
#' files to the work folder and checks them before GGIR sees them.
#'
#' OneDrive keeps many files online only and downloads them when they are
#' first read, which can fail part-way. Files are therefore copied one at a
#' time to a temporary `.partial` file, checked for size, and only then given
#' their final name. Files that fail are retried, with a wait in between to
#' give OneDrive time to download them. A file already copied in full is not
#' copied again.
#'
#' Copies are named `<record_id>_<timepoint>_<serial>-<download time>.bin`, so
#' GGIR reads the record ID as the participant ID, the files of a recording
#' share an ID, and a copy is only reused for the file it was made from. Files
#' left in the recording's folder from earlier runs are removed.
#'
#' Each copy is then checked using its GENEActiv header:
#' * it must be at least 2 MB, as GGIR skips smaller files;
#' * it must contain every page the header lists, which catches files that
#'   were only partly downloaded from the device or uploaded to OneDrive;
#' * its timezone must be one used by `config$tz`. A device set up on a
#'   computer in another timezone would have all its times shifted.
#'
#' The recording must start no more than `window_days` before visit 1, and
#' before visit 2, or it may be in the wrong timepoint folder.
#'
#' When a recording has several files, they are a device swap, and are
#' appended by GGIR, if each device started 24 hours or more after the one
#' before and no more than 3 days after it stopped. Otherwise the recording
#' needs review:
#' * devices started within 24 hours of each other may have been worn at the
#'   same time (e.g. by a parent);
#' * a device started more than 3 days after the one before stopped looks
#'   like a second attempt at the recording rather than a swap.
#'
#' To resolve these, leave only the file(s) to use and move the others to an
#' `Excluded` subfolder.
#'
#' @param jobs A job table from [kv_locate()].
#' @param config A [kv_config()] object.
#' @param retries How many times to retry files that fail to copy.
#' @param retry_wait Seconds to wait before each retry.
#' @param window_days How many days before visit 1 a recording may start.
#'   Devices are set up shortly before they are handed over.
#'
#' @return The job table, with each recording's folder in the work folder in
#'   `job_dir` and, in `files`, the path of each copy in `local` and details
#'   from its header. Recordings whose files were copied and passed the checks
#'   have status `"ready"`; the others have status `"copy_failed"`,
#'   `"invalid_file"` or `"needs_review"`.
#' @export
kv_fetch <- function(jobs, config = kv_config(), retries = 5,
                     retry_wait = 60, window_days = 3) {
  if (!"job_dir" %in% names(jobs)) jobs$job_dir <- NA_character_
  rows <- which(jobs$status == "located")
  if (length(rows) == 0) {
    return(report_jobs(jobs, "Fetch", rows))
  }

  for (i in rows) {
    name <- job_name(jobs, i)
    jobs$job_dir[[i]] <- file.path(config$work_dir, "jobs", name)
    files <- jobs$files[[i]]
    raw_dir <- file.path(jobs$job_dir[[i]], "raw")
    files$local <- file.path(raw_dir, paste0(name, "_", copy_id(files), ".bin"))
    jobs$files[i] <- list(files)
    if (fs::dir_exists(raw_dir)) {
      fs::file_delete(setdiff(fs::dir_ls(raw_dir), files$local))
    }
  }

  copies <- dplyr::bind_rows(jobs$files[rows], .id = "row")
  copies$row <- rows[as.integer(copies$row)]
  check_space_for(copies, config$work_dir)
  copies$error <- copy_with_retry(
    copies$path, copies$local,
    retries = retries, retry_wait = retry_wait
  )

  for (i in rows) {
    failed <- copies[copies$row == i & !is.na(copies$error), ]
    if (nrow(failed) > 0) {
      jobs <- set_status(jobs, i, "copy_failed", paste0(
        "Could not copy ", basename(failed$path), ": ", failed$error,
        collapse = "; "
      ))
      next
    }
    jobs <- check_copies(jobs, i, config$tz, window_days)
  }

  report_jobs(jobs, "Fetch", rows)
}

# Identifies each source file in its copy's name: the device serial and
# download time where the name gives them, or else a hash of its path.
copy_id <- function(files) {
  ifelse(
    !is.na(files$serial) & !is.na(files$downloaded),
    paste0(files$serial, "-", format(files$downloaded, "%Y%m%d%H%M%S")),
    substr(vapply(files$path, rlang::hash, character(1)), 1, 12)
  )
}

check_space_for <- function(copies, work_dir) {
  fs::dir_create(work_dir)
  done <- is_copied(copies$local, copies$size)
  needed_gb <- sum(copies$size[!done]) / 1e9
  free_gb <- disk_free_gb(work_dir)
  if (needed_gb > free_gb) {
    cli::cli_abort(c(
      "Not enough space in {.path {work_dir}}.",
      "i" = "The files need {round(needed_gb, 1)} GB, but only \\
      {round(free_gb, 1)} GB is free."
    ))
  }
}

# Checks the copied files of job `i` and sets its status.
check_copies <- function(jobs, i, tz, window_days) {
  files <- jobs$files[[i]]
  header <- purrr::list_rbind(lapply(files$local, inspect_bin, tz = tz))
  files <- dplyr::bind_cols(files, header[setdiff(names(header), "path")])
  files <- files[order(files$header_start), ]
  jobs$files[i] <- list(files)

  for (status in c("invalid_file", "needs_review")) {
    bad <- files$check == status
    if (any(bad)) {
      return(set_status(
        jobs, i, status, paste(files$problem[bad], collapse = " ")
      ))
    }
  }

  problem <- check_start_times(
    files, jobs$visit1_date[[i]], jobs$visit2_date[[i]], tz, window_days
  )
  if (!is.na(problem)) {
    return(set_status(jobs, i, "needs_review", problem))
  }

  for (note in copy_notes(files, jobs$device_num[[i]])) {
    jobs <- add_note(jobs, i, note)
  }
  set_status(jobs, i, "ready")
}

# Returns why the recording's start times need review, or NA if they don't.
check_start_times <- function(files, visit1, visit2, tz, window_days) {
  name <- basename(files$path)
  when <- format(files$header_start, "%Y-%m-%d %H:%M")
  ended <- format(files$header_end, "%Y-%m-%d %H:%M")

  if (nrow(files) > 1) {
    gaps <- as.numeric(diff(files$header_start), units = "hours")
    close <- which(gaps < 24)
    if (length(close) > 0) {
      pairs <- paste0(
        name[close], " (", when[close], ") and ", name[close + 1],
        " (", when[close + 1], ")"
      )
      return(paste0(
        paste(pairs, collapse = "; "), " started less than 24 hours apart, ",
        "so the devices may have been worn at the same time. Leave only the ",
        "child's file(s) and move the others to an \"", excluded_folder,
        "\" subfolder."
      ))
    }

    gaps <- as.numeric(
      files$header_start[-1] - files$header_end[-nrow(files)],
      units = "days"
    )
    apart <- which(gaps > max_swap_gap_days)
    if (length(apart) > 0) {
      j <- apart[[1]]
      return(paste0(
        name[j], " stopped recording at ", ended[j], " and ", name[j + 1],
        " started at ", when[j + 1], ", ", round(gaps[j], 1), " days later, ",
        "so they look like two attempts at the recording rather than a ",
        "device swap. Leave the file(s) to use and move the others to an \"",
        excluded_folder, "\" subfolder."
      ))
    }
  }

  start <- local_date(files$header_start)
  early <- !is.na(visit1) & start < visit1 - window_days
  late <- !is.na(visit2) & !is.na(visit1) & visit2 >= visit1 & start > visit2
  outside <- early | late
  if (any(outside)) {
    return(paste0(
      paste(name[outside], collapse = ", "), " started recording on ",
      paste(start[outside], collapse = ", "), ", but visit 1 was ", visit1,
      " and visit 2 was ", visit2, ". Is it in the right timepoint folder, ",
      "and are the visit dates right?"
    ))
  }
  NA_character_
}

# Things worth recording about the copies that don't stop processing.
copy_notes <- function(files, device_num) {
  notes <- character()
  name <- basename(files$path)

  mismatch <- !is.na(files$serial) & !is.na(files$header_serial) &
    files$serial != files$header_serial
  notes <- c(notes, glue::glue(
    "{name[mismatch]} is named with serial {files$serial[mismatch]}, but its ",
    "header says {files$header_serial[mismatch]}."
  ))

  device <- suppressWarnings(as.integer(device_num))
  serials <- suppressWarnings(as.integer(files$header_serial))
  if (!is.na(device) && !all(is.na(serials)) && !device %in% serials) {
    notes <- c(notes, glue::glue(
      "The GENEActiv number in REDCap ({device_num}) doesn't match the ",
      "device serial ({paste(files$header_serial, collapse = ', ')})."
    ))
  }

  if (nrow(files) > 1) {
    notes <- c(notes, glue::glue(
      "{nrow(files)} files from a device swap are appended."
    ))
    overlap <- as.numeric(
      files$header_end[-nrow(files)] - files$header_start[-1],
      units = "hours"
    )
    for (j in which(overlap > 0)) {
      notes <- c(notes, glue::glue(
        "{name[j]} and {name[j + 1]} overlap by {round(overlap[j], 1)} ",
        "hours; GGIR uses the later file there."
      ))
    }
  }
  as.character(notes)
}

# The longest gap between one device stopping and the next starting that is
# still treated as a device swap.
max_swap_gap_days <- 3

# Copies files, retrying failures. Returns the error message for each file,
# or NA where it was copied.
copy_with_retry <- function(from, to, retries = 5, retry_wait = 60) {
  errors <- copy_files(from, to)
  for (attempt in seq_len(retries)) {
    failed <- which(!is.na(errors))
    if (length(failed) == 0) break
    cli::cli_inform(c("!" = paste(
      "{length(failed)} file{?s} failed to copy. Retrying in {retry_wait}",
      "seconds ({attempt}/{retries})."
    )))
    Sys.sleep(retry_wait)
    errors[failed] <- copy_files(from[failed], to[failed])
  }
  errors
}

copy_files <- function(from, to) {
  progress <- byte_progress(sum(file.size(from), na.rm = TRUE))
  errors <- vapply(seq_along(from), function(j) {
    tryCatch(
      {
        copy_file(from[[j]], to[[j]], progress = progress)
        NA_character_
      },
      error = function(e) conditionMessage(e)
    )
  }, character(1))
  errors
}

# Starts a progress bar for copying `total` bytes, which lasts until the
# calling function returns, and returns a function that adds bytes to it.
# Increments are passed as doubles: cli counts integer increments in an
# integer, which overflows after 2 GB.
byte_progress <- function(total, .envir = parent.frame()) {
  bar <- cli::cli_progress_bar(
    total = as.numeric(total),
    format = paste(
      "{cli::pb_spin} Copying {cli::pb_current_bytes}/{cli::pb_total_bytes}",
      "[{cli::pb_rate_bytes}] {cli::pb_eta_str}"
    ),
    .envir = .envir
  )
  function(n) {
    cli::cli_progress_update(id = bar, inc = as.numeric(n), .envir = .envir)
  }
}

# Whether each copy exists with its source's size, so needn't be copied again.
is_copied <- function(to, size) (file.size(to) == size) %in% TRUE

# Copies one file in chunks, so progress can be shown and a failed read
# leaves nothing behind but an incomplete `.partial` file, which is removed.
copy_file <- function(from, to, progress = function(n) NULL,
                      chunk_size = 16 * 1024^2) {
  size <- file.size(from)
  if (is.na(size)) {
    stop("the file can't be found or read.", call. = FALSE)
  }
  if (is_copied(to, size)) {
    progress(size)
    return(invisible(to))
  }

  fs::dir_create(dirname(to))
  partial <- paste0(to, ".partial")
  on.exit(unlink(partial), add = TRUE)
  input <- file(from, open = "rb")
  on.exit(close(input), add = TRUE)
  output <- file(partial, open = "wb")
  tryCatch(
    repeat {
      chunk <- readBin(input, "raw", n = chunk_size)
      if (length(chunk) == 0) break
      writeBin(chunk, output)
      progress(length(chunk))
    },
    finally = close(output)
  )

  copied <- file.size(partial)
  if (copied != size) {
    stop(sprintf("only %.0f of %.0f bytes were read.", copied, size),
      call. = FALSE
    )
  }
  if (!file.rename(partial, to)) {
    stop("the copy couldn't be renamed to ", basename(to), ".", call. = FALSE)
  }
  invisible(to)
}

#' Check a GENEActiv file's header
#'
#' Reads the header and last page of a GENEActiv `.bin` file and checks that
#' GGIR can use it. Used by [kv_fetch()] on each copied file.
#'
#' @param path Path to a GENEActiv `.bin` file.
#' @param tz Olson name of the timezone the devices should be set up in.
#' @param min_mb Smallest file size GGIR processes, in MB.
#'
#' @return A one-row tibble. `check` is `"ok"`, `"invalid_file"` or
#'   `"needs_review"`, with the reason in `problem`. The other columns give
#'   the device serial, sample rate, timezone offset in hours, number of
#'   pages, and the recording's start and end times.
#' @export
inspect_bin <- function(path, tz = "Australia/Sydney", min_mb = 2) {
  name <- basename(path)
  out <- tibble::tibble(
    path = path,
    check = "ok",
    problem = NA_character_,
    header_serial = NA_character_,
    sample_rate = NA_real_,
    tz_offset = NA_real_,
    pages = NA_real_,
    header_start = as.POSIXct(NA, tz = tz),
    header_end = as.POSIXct(NA, tz = tz)
  )
  problem <- function(check, message) {
    out$check <- check
    out$problem <- message
    out
  }

  size <- file.size(path)
  if (is.na(size) || size < min_mb * 1e6) {
    return(problem("invalid_file", glue::glue(
      "{name} is {round(size / 1e6, 2)} MB, smaller than the {min_mb} MB ",
      "GGIR needs."
    )))
  }

  # Checked first, as GGIRread doesn't reliably reject other files
  if (!identical(readBin(path, "raw", 15), charToRaw("Device Identity"))) {
    return(problem("invalid_file", glue::glue(
      "{name} isn't a GENEActiv .bin file."
    )))
  }
  header <- tryCatch(
    suppressWarnings(GGIRread::readGENEActiv(
      path, start = 1, end = 1, desiredtz = tz, configtz = tz
    ))$header,
    error = function(e) e
  )
  if (inherits(header, "error")) {
    return(problem("invalid_file", glue::glue(
      "{name} can't be read as a GENEActiv file: {conditionMessage(header)}"
    )))
  }

  out$header_serial <- header$serial_number
  out$sample_rate <- header$SampleRate
  out$tz_offset <- header$tzone / 3600
  out$pages <- header$numBlocksTotal
  out$header_start <- as.POSIXct(
    sub(":([0-9]+)$", ".\\1", header$StartTime),
    format = "%Y-%m-%d %H:%M:%OS", tz = tz
  )
  # Each page holds 300 samples
  out$header_end <- out$header_start + out$pages * 300 / out$sample_rate
  if (is.na(out$header_end)) {
    return(problem("invalid_file", glue::glue(
      "{name}'s header doesn't give its start time, sample rate and number ",
      "of pages."
    )))
  }

  last <- last_page_number(path)
  if (is.na(last) || last < out$pages - 1) {
    return(problem("invalid_file", glue::glue(
      "{name} is incomplete: it has {if (is.na(last)) 0 else last + 1} of ",
      "{out$pages} pages. Download it from the device again or re-upload it."
    )))
  }

  year <- format(out$header_start, "%Y")
  if (!header$tzone %in% tz_offsets(tz, year)) {
    return(problem("needs_review", glue::glue(
      "{name} was set up in timezone GMT{sprintf('%+g', out$tz_offset)}, ",
      "not {tz} time, so its times would be shifted."
    )))
  }

  out
}

# The sequence number of the last page in a GENEActiv file (numbered from 0).
last_page_number <- function(path, bytes = 65536) {
  size <- file.size(path)
  con <- file(path, open = "rb")
  on.exit(close(con))
  seek(con, max(0, size - bytes))
  tail <- readBin(con, "raw", n = bytes)
  pages <- grepRaw("Sequence Number:[0-9]+", tail, all = TRUE, value = TRUE)
  if (length(pages) == 0) {
    return(NA_real_)
  }
  as.numeric(sub("Sequence Number:", "", rawToChar(pages[[length(pages)]])))
}

# The UTC offsets, in seconds, that a timezone uses in a year (standard and
# daylight saving time).
tz_offsets <- function(tz, year) {
  times <- as.POSIXct(paste0(year, c("-01-15", "-07-15"), " 12:00"), tz = tz)
  offset <- format(times, "%z")
  sign <- ifelse(substr(offset, 1, 1) == "-", -1, 1)
  hours <- as.numeric(substr(offset, 2, 3))
  minutes <- as.numeric(substr(offset, 4, 5))
  unique(sign * (hours * 3600 + minutes * 60))
}
