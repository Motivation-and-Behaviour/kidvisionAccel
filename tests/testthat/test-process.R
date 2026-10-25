start_at <- function(x) as.POSIXct(x, tz = "Australia/Sydney")

test_that("study_dates() uses visit 1 to visit 2 when they are plausible", {
  dates <- study_dates(
    as.Date("2022-09-29"), as.Date("2022-10-07"), start_at("2022-09-29 11:50")
  )

  expect_equal(dates$first, as.Date("2022-09-29"))
  expect_equal(dates$last, as.Date("2022-10-07"))
  expect_true(is.na(dates$reason))
})

test_that("study_dates() uses the whole recording when dates look wrong", {
  v1 <- as.Date("2025-03-10")
  start <- start_at("2025-03-10 11:50")

  expect_match(study_dates(NA, v1, start)$reason, "visit date is missing")
  expect_match(
    study_dates(v1, v1 + 30, start)$reason, "visit 2 was 30 days after"
  )
  expect_match(
    study_dates(v1, v1 - 100, start)$reason, "visit 2 was -100 days after"
  )
  # A recording starting after visit 1 means the visit 1 date may be wrong,
  # and masking from it could lose real data
  expect_match(
    study_dates(v1 - 2, v1 + 7, start)$reason, "started on 2025-03-10"
  )
  expect_true(is.na(study_dates(v1 + 2, v1 + 9, start)$reason))
})

check <- function(ggir_dir, record_id = "9999", n_files = 1, error = NULL) {
  check_ggir_output(ggir_dir, record_id, n_files, error, "x.log")
}

test_that("check_ggir_output() accepts complete GGIR output", {
  expect_equal(check(local_ggir_output())$status, "processed")
})

test_that("check_ggir_output() reports GGIR errors and timeouts", {
  error <- structure(
    class = c("callr_status_error", "error", "condition"),
    list(message = "callr failed", parent = simpleError("bad header\nmore"))
  )
  timeout <- structure(
    class = c("callr_timeout_error", "error", "condition"),
    list(message = "timed out")
  )
  ggir_dir <- local_ggir_output(stage = 2)

  failed <- check(ggir_dir, error = error)
  expect_equal(failed$status, "ggir_failed")
  expect_equal(
    failed$message,
    "GGIR stopped with an error after part 2: bad header. See x.log."
  )
  timed_out <- check(ggir_dir, error = timeout)
  expect_equal(timed_out$status, "ggir_timeout")
  expect_match(timed_out$message, "didn't finish in time")
})

test_that("check_ggir_output() reports missing part 2 results", {
  ggir_dir <- local_ggir_output(
    function(results) file.remove(file.path(results, "part2_summary.csv")),
    stage = 1
  )
  outcome <- check(ggir_dir)

  expect_equal(outcome$status, "ggir_failed")
  expect_match(
    outcome$message, "stopped after part 1 without writing part2_summary.csv"
  )
})

test_that("check_ggir_output() accepts output without part 4, 5 or 6", {
  # GGIR writes these only when a night or day is valid
  ggir_dir <- local_ggir_output(function(results) {
    parts <- c("part4_*", "part5_*", "part6_*")
    file.remove(Sys.glob(file.path(results, parts)))
  })

  expect_equal(check(ggir_dir)$status, "processed")
})

test_that("check_ggir_output() reports files GGIR found corrupt", {
  ggir_dir <- local_ggir_output(function(results) {
    edit_result(results, "QC/data_quality_report.csv", "file.corrupt", "TRUE")
  })
  outcome <- check(ggir_dir)

  expect_equal(outcome$status, "invalid_file")
  expect_match(outcome$message, "flags 9999_baseline_test.csv as corrupt")
})

test_that("check_ggir_output() checks the ID and that files were appended", {
  ggir_dir <- local_ggir_output()
  expect_match(check(ggir_dir, "1001")$message, "ID as 9999, not 1001")

  ggir_dir <- local_ggir_output(function(results) {
    edit_result(results, "part2_daysummary.csv", "filename",
      c("a.bin", "a.bin", "b.bin", "b.bin", "b.bin")
    )
  })
  outcome <- check(ggir_dir, n_files = 2)
  expect_equal(outcome$status, "needs_review")
  expect_match(outcome$message, "separate recordings")
})

test_that("ggir_stage() describes how far GGIR got", {
  expect_equal(ggir_stage(local_ggir_output(stage = 0)), "in part 1")
  expect_equal(ggir_stage(local_ggir_output(stage = 4)), "after part 4")
})

ready_job <- function(work_dir, visit1 = NA, visit2 = NA) {
  jobs <- fake_jobs("9999", visit1_date = visit1, visit2_date = visit2)
  jobs$status <- "ready"
  jobs$job_dir <- file.path(work_dir, "jobs", "9999_baseline")
  jobs$files <- list(tibble::tibble(
    local = file.path(jobs$job_dir, "raw", "9999_baseline_x.bin"),
    header_start = as.POSIXct("2016-06-23 09:00", tz = "Australia/Sydney")
  ))
  jobs
}

test_that("kv_process() runs GGIR and checks its output", {
  work_dir <- withr::local_tempdir()
  calls <- list()
  # Stands in for GGIR: copies its output for the synthetic recording
  local_mocked_bindings(start_ggir = function(args, log) {
    calls[[length(calls) + 1]] <<- args
    fs::dir_copy(
      local_ggir_output(env = parent.frame(3)),
      file.path(args$outputdir, paste0("output_", args$studyname))
    )
    callr::r_bg(function() NULL)
  })
  jobs <- ready_job(work_dir, "2016-06-23", "2016-06-30")

  jobs <- suppressMessages(kv_process(jobs, kv_config(work_dir = work_dir)))

  expect_equal(jobs$status, "processed")
  expect_equal(
    jobs$ggir_dir, file.path(jobs$job_dir, "ggir", "output_9999_baseline")
  )
  expect_equal(jobs$study_first, as.Date("2016-06-23"))
  expect_equal(jobs$study_last, as.Date("2016-06-30"))
  expect_match(jobs$notes[[1]], "Used data from 2016-06-23 to 2016-06-30")

  args <- calls[[1]]
  expect_equal(args$datadir, jobs$files[[1]]$local)
  expect_equal(args$studyname, "9999_baseline")
  expect_equal(args$desiredtz, "Australia/Sydney")
  expect_equal(
    read.csv(args$study_dates_file),
    data.frame(ID = 9999, first = "2016-06-23", last = "2016-06-30")
  )
})

test_that("kv_process() uses the whole recording without plausible dates", {
  work_dir <- withr::local_tempdir()
  local_mocked_bindings(start_ggir = function(args, log) {
    expect_null(args$study_dates_file)
    callr::r_bg(function() stop("GGIR crashed"))
  })

  jobs <- suppressMessages(
    kv_process(ready_job(work_dir), kv_config(work_dir = work_dir))
  )

  expect_equal(jobs$status, "ggir_failed")
  expect_match(jobs$message, "GGIR crashed")
  expect_match(jobs$notes[[1]], "whole recording, as a visit date is missing")
})

# Background processes that stand in for GGIR: each run's `args` say how long
# to take and whether to fail.
local_fake_ggir <- function(env = parent.frame()) {
  local_mocked_bindings(
    start_ggir = function(args, log) {
      callr::r_bg(
        function(wait, fail) {
          Sys.sleep(wait)
          if (fail) stop("bad file")
        },
        args = list(wait = args$wait, fail = args$fail)
      )
    },
    .env = env
  )
}

fake_run <- function(name, wait = 0, fail = FALSE) {
  list(name = name, args = list(wait = wait, fail = fail), log = "x.log")
}

test_that("kv_process() rejects an invalid worker count", {
  config <- kv_config(work_dir = withr::local_tempdir())

  expect_error(
    kv_process(fake_jobs("9999"), config, workers = 0),
    "positive whole number"
  )
})

test_that("run_ggir_jobs() runs recordings in parallel", {
  local_fake_ggir()
  runs <- list(fake_run("a", 2), fake_run("b", 2), fake_run("c", 0))

  started <- Sys.time()
  errors <- suppressMessages(run_ggir_jobs(runs, workers = 3, timeout = 1))
  elapsed <- as.numeric(Sys.time() - started, units = "secs")

  expect_equal(errors, list(NULL, NULL, NULL))
  expect_lt(elapsed, 3.5)
})

test_that("run_ggir_jobs() returns each recording's error", {
  local_fake_ggir()
  runs <- list(fake_run("a"), fake_run("b", fail = TRUE))

  errors <- suppressMessages(run_ggir_jobs(runs, workers = 1, timeout = 1))

  expect_null(errors[[1]])
  expect_match(conditionMessage(errors[[2]]$parent), "bad file")
})

test_that("run_ggir_jobs() stops recordings that take too long", {
  local_fake_ggir()
  runs <- list(fake_run("slow", wait = 30), fake_run("quick"))

  errors <- suppressMessages(
    run_ggir_jobs(runs, workers = 2, timeout = 2 / 60)
  )

  expect_s3_class(errors[[1]], "callr_timeout_error")
  expect_null(errors[[2]])
})
