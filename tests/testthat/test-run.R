# Replaces every step with one that records its call and moves recordings
# along, so kv_run()'s orchestration can be tested without REDCap or GGIR.
local_fake_steps <- function(statuses, env = parent.frame()) {
  calls <- new.env()
  calls$steps <- character()
  calls$batches <- list()
  step <- function(name, from, to) {
    function(jobs, config, ...) {
      calls$steps <- c(calls$steps, name)
      if (name == "fetch") calls$batches[[length(calls$batches) + 1]] <-
        jobs$record_id
      jobs$status[jobs$status == from] <- to
      jobs
    }
  }
  local_mocked_bindings(
    kv_check = function(config, dry_run) calls$dry_run <- dry_run,
    kv_pending = function(config, ...) {
      jobs <- fake_jobs(names(statuses))
      jobs$job_dir <- NA_character_
      jobs
    },
    kv_locate = function(jobs, config) {
      jobs$status <- unname(statuses[jobs$record_id])
      jobs
    },
    kv_fetch = step("fetch", "located", "ready"),
    kv_process = step("process", "ready", "processed"),
    kv_collate = step("collate", "processed", "collated"),
    kv_report = step("report", "collated", "reported"),
    kv_write = function(jobs, config, dry_run) {
      calls$steps <- c(calls$steps, "write")
      jobs$status[jobs$status == "reported"] <- "written"
      jobs$redcap_status <- ifelse(jobs$status == "written", 1L, 3L)
      jobs
    },
    kv_cleanup = step("cleanup", "", ""),
    .env = env
  )
  calls
}

test_that("kv_run() processes located recordings in batches", {
  calls <- local_fake_steps(c(
    "1001" = "located", "1002" = "no_files", "1003" = "located",
    "1004" = "located"
  ))
  work_dir <- withr::local_tempdir()

  jobs <- suppressMessages(
    kv_run(kv_config(work_dir = work_dir), workers = 2, dry_run = TRUE)
  )

  expect_true(calls$dry_run)
  expect_equal(jobs$record_id, c("1001", "1002", "1003", "1004"))
  expect_equal(jobs$status, c("written", "no_files", "written", "written"))
  expect_equal(calls$batches, list(c("1001", "1003"), "1004"))
  # The recording without files is written first, then each batch
  expect_equal(calls$steps[1], "write")
  expect_equal(sum(calls$steps == "cleanup"), 2)

  log <- fs::dir_ls(file.path(work_dir, "logs"), glob = "*run_*.csv")
  expect_length(log, 1)
  expect_equal(read.csv(log)$redcap_status, c(1, 3, 1, 1))
})

test_that("kv_run() can keep the local copies", {
  calls <- local_fake_steps(c("1001" = "located"))

  suppressMessages(kv_run(
    kv_config(work_dir = withr::local_tempdir()), cleanup = FALSE
  ))

  expect_false("cleanup" %in% calls$steps)
})

test_that("kv_run() stops early when nothing needs processing", {
  local_mocked_bindings(
    kv_check = function(config, dry_run) NULL,
    kv_pending = function(config, ...) fake_jobs(character()),
    kv_locate = function(...) stop("shouldn't be called")
  )

  jobs <- kv_run(kv_config(work_dir = withr::local_tempdir()))

  expect_equal(nrow(jobs), 0)
})

test_that("kv_run() rejects an invalid worker count before starting", {
  local_mocked_bindings(kv_check = function(...) stop("shouldn't be called"))

  expect_error(
    kv_run(kv_config(work_dir = withr::local_tempdir()), workers = 0),
    "positive whole number"
  )
})

test_that("kv_cleanup() deletes each recording's local folder", {
  work_dir <- withr::local_tempdir()
  folders <- file.path(work_dir, "jobs", c("1001_baseline", "1002_baseline"))
  fs::dir_create(file.path(folders, "raw"))
  fs::dir_create(file.path(work_dir, "logs"))
  jobs <- fake_jobs(c("1001", "1002", "1003"))
  jobs$job_dir <- c(folders, NA)

  kv_cleanup(jobs, kv_config(work_dir = work_dir))

  expect_false(any(fs::dir_exists(folders)))
  expect_true(fs::dir_exists(file.path(work_dir, "logs")))
})
