# Builds a fake OneDrive source folder. `files` are relative paths to create
# with a few bytes of content, `empty` relative paths to create empty, and
# `dirs` extra folders.
local_source <- function(files = character(), empty = character(),
                         dirs = character(), env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  fs::dir_create(file.path(root, dirs))
  for (f in c(files, empty)) {
    fs::dir_create(dirname(file.path(root, f)))
  }
  for (f in files) writeBin(charToRaw("data"), file.path(root, f))
  for (f in empty) file.create(file.path(root, f))
  root
}

# A GENEActiv file name: "<subject>__<serial>_<download time>.bin".
bin_name <- function(subject, serial = "029501",
                     downloaded = "2025-03-20 14-43-54") {
  paste0(subject, "__", serial, "_", downloaded, ".bin")
}

# A job table as kv_pending() returns it.
fake_jobs <- function(record_id, timepoint = "baseline", visit1_date = NA,
                      visit2_date = NA, device_num = NA_character_) {
  n <- length(record_id)
  tibble::tibble(
    record_id = record_id,
    timepoint = rep_len(timepoint, n),
    event = timepoint_map$visit2_event[
      match(rep_len(timepoint, n), timepoint_map$timepoint)
    ],
    reason = "new",
    visit1_date = as.Date(rep_len(visit1_date, n)),
    visit2_date = as.Date(rep_len(visit2_date, n)),
    accel_downloaded = TRUE,
    device_num = rep_len(device_num, n),
    status = "pending",
    message = NA_character_,
    notes = rep(list(character()), n)
  )
}

# GGIRread's GENEActiv test file has 17 pages, recorded from 2013-05-30
# 10:12:54 at 85.7 Hz, but its header claims 222048 pages and a GMT+1
# timezone. write_geneactiv() copies it with the header edited, so tests can
# make complete or incomplete files.
geneactiv_testfile <- function() {
  system.file("testfiles", "GENEActiv_testfile.bin", package = "GGIRread")
}

write_geneactiv <- function(path, pages = 17, tz = "+10:00", start = NULL) {
  raw <- readBin(geneactiv_testfile(), "raw", file.size(geneactiv_testfile()))
  raw <- replace_raw(raw, "Number of Pages:222048", paste0(
    "Number of Pages:", pages
  ))
  raw <- replace_raw(raw, "Time Zone:GMT +01:00", paste0("Time Zone:GMT ", tz))
  if (!is.null(start)) {
    raw <- replace_raw(raw, "Page Time:2013-05-30 10:12:54", paste0(
      "Page Time:", start
    ))
  }
  fs::dir_create(dirname(path))
  writeBin(raw, path)
  path
}

# Replaces the first occurrence of `old` in a raw vector.
replace_raw <- function(raw, old, new) {
  at <- grepRaw(old, raw, fixed = TRUE)
  stopifnot(length(at) == 1)
  c(raw[seq_len(at - 1)], charToRaw(new), raw[-seq_len(at - 1 + nchar(old))])
}
