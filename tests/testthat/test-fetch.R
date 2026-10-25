tz <- "Australia/Sydney"

test_that("copy_file() copies in chunks and reports progress", {
  dir <- withr::local_tempdir()
  from <- file.path(dir, "from.bin")
  writeBin(as.raw(1:250), from)
  to <- file.path(dir, "sub", "to.bin")
  progress <- 0

  copy_file(from, to, progress = function(n) progress <<- progress + n,
    chunk_size = 100
  )

  expect_identical(readBin(to, "raw", 1000), as.raw(1:250))
  expect_equal(progress, 250)
  expect_false(file.exists(paste0(to, ".partial")))
})

test_that("copy_file() leaves nothing behind when a copy fails", {
  dir <- withr::local_tempdir()
  from <- file.path(dir, "from.bin")
  writeBin(as.raw(1:250), from)
  to <- file.path(dir, "to.bin")

  expect_error(copy_file(
    from, to, progress = function(n) stop("connection lost"), chunk_size = 100
  ), "connection lost")
  expect_false(file.exists(to))
  expect_false(file.exists(paste0(to, ".partial")))
})

test_that("copy_file() doesn't copy a file again", {
  dir <- withr::local_tempdir()
  from <- file.path(dir, "from.bin")
  to <- file.path(dir, "to.bin")
  writeBin(as.raw(1:10), from)
  writeBin(as.raw(11:20), to)

  copy_file(from, to)

  expect_identical(readBin(to, "raw", 100), as.raw(11:20))
})

test_that("byte_progress() counts past 2 GB", {
  copy <- function() {
    progress <- byte_progress(3e9)
    for (i in 1:3) progress(as.integer(1e9))
  }

  expect_no_warning(expect_no_error(copy()))
})

test_that("copy_with_retry() retries files that fail", {
  calls <- list()
  local_mocked_bindings(copy_files = function(from, to) {
    calls[[length(calls) + 1]] <<- from
    if (length(calls) == 1) c(NA, "timed out") else NA_character_
  })

  errors <- suppressMessages(
    copy_with_retry(c("a", "b"), c("x", "y"), retries = 3, retry_wait = 0)
  )

  expect_equal(errors, c(NA_character_, NA_character_))
  expect_equal(calls, list(c("a", "b"), "b"))
})

test_that("copy_with_retry() gives up after the last retry", {
  local_mocked_bindings(copy_files = function(from, to) {
    rep("timed out", length(from))
  })

  errors <- suppressMessages(
    copy_with_retry("a", "x", retries = 2, retry_wait = 0)
  )

  expect_equal(errors, "timed out")
})

test_that("inspect_bin() accepts a complete file set up in Sydney time", {
  path <- write_geneactiv(file.path(withr::local_tempdir(), "ok.bin"))
  info <- inspect_bin(path, tz, min_mb = 0)

  expect_equal(info$check, "ok")
  expect_equal(info$header_serial, "012967")
  expect_equal(info$sample_rate, 85.7)
  expect_equal(info$tz_offset, 10)
  expect_equal(info$pages, 17)
  expect_equal(format(info$header_start), "2013-05-30 10:12:54")
  expect_equal(
    as.numeric(info$header_end - info$header_start, units = "secs"),
    17 * 300 / 85.7
  )
})

test_that("inspect_bin() rejects files GGIR can't use", {
  dir <- withr::local_tempdir()

  expect_match(
    inspect_bin(geneactiv_testfile(), tz)$problem, "smaller than the 2 MB"
  )
  expect_match(
    inspect_bin(geneactiv_testfile(), tz, min_mb = 0)$problem,
    "incomplete: it has 17 of 222048 pages"
  )

  junk <- file.path(dir, "junk.bin")
  writeBin(as.raw(sample(0:255, 5000, replace = TRUE)), junk)
  expect_match(
    inspect_bin(junk, tz, min_mb = 0)$problem, "isn't a GENEActiv .bin file"
  )

  no_start <- write_geneactiv(file.path(dir, "no_start.bin"), start = "x")
  info <- inspect_bin(no_start, tz, min_mb = 0)
  expect_equal(info$check, "invalid_file")
  expect_match(info$problem, "doesn't give its start time")
})

test_that("inspect_bin() flags devices set up in another timezone", {
  path <- write_geneactiv(
    file.path(withr::local_tempdir(), "uk.bin"), tz = "+01:00"
  )
  info <- inspect_bin(path, tz, min_mb = 0)

  expect_equal(info$check, "needs_review")
  expect_match(info$problem, "set up in timezone GMT\\+1")
})

test_that("last_page_number() reads the last page's sequence number", {
  expect_equal(last_page_number(geneactiv_testfile()), 16)
})

test_that("tz_offsets() covers standard and daylight saving time", {
  expect_setequal(tz_offsets("Australia/Sydney", "2025"), c(36000, 39600))
  expect_equal(tz_offsets("Australia/Brisbane", "2025"), 36000)
})

two_files <- function(start2, end1 = "2025-03-19 09:00") {
  tibble::tibble(
    path = c("a.bin", "b.bin"),
    header_start = as.POSIXct(c("2025-03-10 09:00", start2), tz = tz),
    header_end = as.POSIXct(c(end1, "2025-03-28 09:00"), tz = tz)
  )
}

test_that("check_start_times() treats overlapping devices as a swap", {
  # The second device started 3 days in, while the first was still recording
  expect_true(is.na(check_start_times(two_files("2025-03-13 09:00"), NA, NA,
    tz, 3)))
  # ...or up to 3 days after the first stopped
  expect_true(is.na(check_start_times(
    two_files("2025-03-15 09:00", end1 = "2025-03-12 12:00"), NA, NA, tz, 3
  )))
})

test_that("check_start_times() flags devices started together", {
  expect_match(
    check_start_times(two_files("2025-03-10 15:00"), NA, NA, tz, 3),
    "started less than 24 hours apart"
  )
})

test_that("check_start_times() flags a second attempt at a recording", {
  rewear <- two_files("2025-03-20 09:00", end1 = "2025-03-14 09:00")

  expect_match(
    check_start_times(rewear, NA, NA, tz, 3),
    "6 days later, so they look like two attempts"
  )
})

test_that("check_start_times() checks the start against the visit dates", {
  files <- tibble::tibble(
    path = "a.bin",
    header_start = as.POSIXct("2025-03-10 09:00", tz = tz),
    header_end = as.POSIXct("2025-03-19 09:00", tz = tz)
  )
  check <- function(visit1, visit2) {
    check_start_times(files, as.Date(visit1), as.Date(visit2), tz, 3)
  }

  expect_true(is.na(check("2025-03-12", "2025-03-20")))
  expect_match(check("2025-03-20", "2025-03-28"), "started recording on")
  expect_match(check("2025-02-20", "2025-03-01"), "started recording on")
  # Visit 2 recorded before visit 1 can't be trusted, so is not used
  expect_true(is.na(check("2025-03-09", "2025-01-01")))
})

test_that("copy_notes() records mismatched serials and overlaps", {
  files <- tibble::tibble(
    path = c("a.bin", "b.bin"),
    serial = c("029501", "029518"),
    header_serial = c("029501", "029999"),
    header_start = as.POSIXct(c("2025-03-10 09:00", "2025-03-13 09:00"),
      tz = tz
    ),
    header_end = as.POSIXct(c("2025-03-14 09:00", "2025-03-20 09:00"),
      tz = tz
    )
  )
  notes <- copy_notes(files, device_num = "12345")

  expect_match(notes, "named with serial 029518, but its header says 029999",
    all = FALSE
  )
  expect_match(notes, "GENEActiv number in REDCap \\(12345\\)", all = FALSE)
  expect_match(notes, "2 files from a device swap", all = FALSE)
  expect_match(notes, "overlap by 24 hours", all = FALSE)
  expect_length(copy_notes(files[1, ], device_num = "29501"), 0)
})

test_that("copy_notes() ignores a header without a serial", {
  files <- tibble::tibble(
    path = "a.bin", serial = "029501", header_serial = NA_character_,
    header_start = as.POSIXct("2025-03-10 09:00", tz = tz),
    header_end = as.POSIXct("2025-03-14 09:00", tz = tz)
  )

  expect_length(copy_notes(files, device_num = "12345"), 0)
})

# kv_fetch() on files written by write_geneactiv(), which are smaller than
# GGIR's minimum, so the size check is relaxed.
local_fetch_mocks <- function(env = parent.frame()) {
  real_inspect <- inspect_bin
  local_mocked_bindings(
    inspect_bin = function(path, tz, ...) real_inspect(path, tz, min_mb = 0),
    disk_free_gb = function(path) 100,
    .env = env
  )
}

located_job <- function(source_files, ...) {
  jobs <- fake_jobs("1001", ...)
  jobs$status <- "located"
  jobs$files <- list(tibble::tibble(
    path = source_files,
    size = file.size(source_files),
    subject = "1001",
    serial = "012967",
    downloaded = as.POSIXct(NA, tz = tz)
  ))
  jobs
}

test_that("kv_fetch() copies and checks a recording's files", {
  local_fetch_mocks()
  source <- write_geneactiv(file.path(withr::local_tempdir(), "x.bin"))
  work_dir <- withr::local_tempdir()
  jobs <- located_job(source, visit1_date = "2013-05-29")

  jobs <- suppressMessages(
    kv_fetch(jobs, kv_config(work_dir = work_dir, tz = tz), retries = 0)
  )

  expect_equal(jobs$status, "ready")
  expect_equal(jobs$job_dir, file.path(work_dir, "jobs", "1001_baseline"))
  local <- jobs$files[[1]]$local
  expect_match(basename(local), "^1001_baseline_[0-9a-f]{12}[.]bin$")
  expect_identical(
    unname(tools::md5sum(local)), unname(tools::md5sum(source))
  )
  expect_equal(jobs$files[[1]]$check, "ok")
})

test_that("kv_fetch() appends a device swap but flags simultaneous devices", {
  local_fetch_mocks()
  dir <- withr::local_tempdir()
  first <- write_geneactiv(file.path(dir, "a.bin"))
  swap <- write_geneactiv(
    file.path(dir, "b.bin"), start = "2013-06-01 10:12:54"
  )
  same_day <- write_geneactiv(
    file.path(dir, "c.bin"), start = "2013-05-30 14:12:54"
  )
  config <- kv_config(work_dir = withr::local_tempdir(), tz = tz)

  swapped <- suppressMessages(
    kv_fetch(located_job(c(first, swap)), config, retries = 0)
  )
  together <- suppressMessages(
    kv_fetch(located_job(c(first, same_day)), config, retries = 0)
  )

  expect_equal(swapped$status, "ready")
  expect_match(swapped$notes[[1]], "2 files from a device swap", all = FALSE)
  expect_equal(together$status, "needs_review")
  expect_match(together$message, "less than 24 hours apart")
  # The swap's second file, left over from the first run, is removed
  expect_setequal(
    fs::dir_ls(file.path(together$job_dir, "raw")),
    together$files[[1]]$local
  )
})

test_that("copy_id() names copies after their source file", {
  files <- tibble::tibble(
    path = c("/a/x.bin", "/a/y.bin"),
    serial = c("029501", NA),
    downloaded = as.POSIXct(c("2025-03-20 14:43:54", NA), tz = tz)
  )
  ids <- copy_id(files)

  expect_equal(ids[1], "029501-20250320144354")
  expect_match(ids[2], "^[0-9a-f]{12}$")
})

test_that("kv_fetch() marks recordings that can't be copied", {
  local_fetch_mocks()
  dir <- withr::local_tempdir()
  source <- write_geneactiv(file.path(dir, "x.bin"))
  jobs <- located_job(source)
  unlink(source)

  jobs <- suppressMessages(kv_fetch(
    jobs, kv_config(work_dir = withr::local_tempdir(), tz = tz),
    retries = 0
  ))

  expect_equal(jobs$status, "copy_failed")
  expect_match(jobs$message, "Could not copy x.bin")
})

test_that("kv_fetch() stops if the work folder is too small", {
  local_mocked_bindings(disk_free_gb = function(path) 0)
  source <- write_geneactiv(file.path(withr::local_tempdir(), "x.bin"))

  expect_error(
    kv_fetch(located_job(source), kv_config(work_dir = withr::local_tempdir())),
    "Not enough space"
  )
})
