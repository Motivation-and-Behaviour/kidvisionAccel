# Runs GGIR for real, which takes about a minute, so only when asked:
# Sys.setenv(KIDVISION_INTEGRATION = "true"); devtools::test(filter = "integration")
skip_if_not(
  identical(Sys.getenv("KIDVISION_INTEGRATION"), "true"),
  "Set KIDVISION_INTEGRATION=true to run GGIR"
)

test_that("kv_process() and kv_collate() process a recording with GGIR", {
  work_dir <- withr::local_tempdir()
  raw_dir <- file.path(work_dir, "jobs", "9999_baseline", "raw")
  fs::dir_create(raw_dir)
  # GGIR's synthetic 4-day test recording, named as kv_fetch() names copies
  GGIR:::create_test_acc_csv(Nmin = 4 * 1440, storagelocation = raw_dir)
  local <- file.path(raw_dir, "9999_baseline_test.csv")
  file.rename(fs::dir_ls(raw_dir), local)

  jobs <- fake_jobs("9999")
  jobs$status <- "ready"
  jobs$job_dir <- dirname(raw_dir)
  jobs$files <- list(tibble::tibble(
    path = local, size = file.size(local), local = local,
    header_start = as.POSIXct(NA)
  ))
  config <- kv_config(work_dir = work_dir, source_dir = raw_dir)

  jobs <- suppressMessages(kv_process(jobs, config, timeout = 10))
  expect_equal(jobs$status, "processed")
  expect_true(file.exists(file.path(work_dir, "logs", "9999_baseline_ggir.log")))

  jobs <- suppressMessages(kv_collate(jobs, config))
  expect_equal(jobs$status, "collated")
  expect_true(all(file.exists(jobs$part_files[[1]])))
  expect_equal(jobs$redcap[[1]]$cac_day2_full_mpa, "24")

  local_mocked_bindings(
    participant_details = fake_participant_details,
    rc_connect = function(config) "rcon",
    rc_validate = function(rcon, data) invisible(data)
  )
  jobs <- suppressMessages(kv_report(jobs, config))
  expect_equal(jobs$status, "reported")

  jobs <- suppressMessages(kv_write(jobs, config, dry_run = TRUE))
  expect_equal(jobs$status, "written")
  dry_run <- file.path(work_dir, "dry_run", "9999_baseline")
  expect_length(fs::dir_ls(dry_run), 8)

  kv_cleanup(jobs, config)
  expect_false(fs::dir_exists(jobs$job_dir))
})
