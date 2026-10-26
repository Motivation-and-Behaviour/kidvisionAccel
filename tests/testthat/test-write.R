test_that("redcap_status_code() maps each outcome to a REDCap status", {
  statuses <- c(
    "reported", "no_folder", "no_files", "no_valid_days", "copy_failed",
    "report_failed", "ggir_timeout", "collate_failed", "misnamed",
    "needs_review", "ggir_failed", "invalid_file", "pending", "collated"
  )

  expect_equal(
    redcap_status_code(statuses),
    c(1L, 3L, 3L, 4L, 5L, 5L, 5L, 5L, 6L, 6L, 7L, 7L, NA, NA)
  )
})

# A recording that has been through kv_report(), with files to upload.
reported_job <- function(work_dir, check_notes = FALSE) {
  out <- file.path(work_dir, "jobs", "9999_baseline", "out")
  fs::dir_create(out)
  paths <- file.path(out, paste0(
    "9999_baseline_", c("part2", "part4", "part5", "part6"), ".csv"
  ))
  file.create(paths)
  report <- file.path(out, "KidVision_9999_baseline.pdf")
  file.create(report)

  jobs <- fake_jobs("9999")
  jobs$status <- "reported"
  jobs$check_notes <- check_notes
  jobs$notes <- list("Used data from 2016-06-23 to 2016-06-30.")
  jobs$files <- list(tibble::tibble(
    path = "/onedrive/9999/Baseline/Accelerometer/x.bin", size = 8.5e8
  ))
  jobs$part_files <- list(stats::setNames(
    paths, c("part2", "part4", "part5", "part6")
  ))
  jobs$report <- report
  jobs$redcap <- list(tibble::tibble(
    record_id = "9999", redcap_event_name = "baseline_visit_2_arm_1",
    cac_sum_mvpa = "33"
  ))
  jobs
}

# Records what kv_write() sends to REDCap.
local_fake_redcap <- function(env = parent.frame()) {
  calls <- new.env()
  calls$log <- list()
  record <- function(type, value) {
    calls$log[[length(calls$log) + 1]] <- list(type = type, value = value)
  }
  local_mocked_bindings(
    rc_connect = function(config) "rcon",
    rc_validate = function(rcon, data) record("validate", data),
    rc_import_records = function(rcon, data) record("records", data),
    rc_import_file = function(rcon, file, record, field, event) {
      record("file", list(file = basename(file), field = field, event = event))
    },
    rc_delete_file = function(rcon, record, field, event) {
      record("delete", list(field = field, event = event))
    },
    .env = env
  )
  calls
}

config_for <- function(work_dir) {
  kv_config(work_dir = work_dir, source_dir = "/onedrive")
}

test_that("kv_write() writes results in order, ending with the status", {
  calls <- local_fake_redcap()
  work_dir <- withr::local_tempdir()

  jobs <- suppressMessages(
    kv_write(reported_job(work_dir), config_for(work_dir))
  )

  expect_equal(jobs$status, "written")
  expect_equal(jobs$redcap_status, 1L)
  types <- vapply(calls$log, `[[`, "", "type")
  expect_equal(types, c(
    "validate", "validate", "records", "records", rep("file", 5), "records"
  ))

  imports <- Filter(function(x) x$type == "records", calls$log)
  expect_equal(imports[[1]]$value$cac_proc_status, "0")
  expect_equal(imports[[2]]$value$cac_sum_mvpa, "33")
  final <- imports[[3]]$value
  expect_equal(final$cac_proc_status, "1")
  expect_equal(final$cac_proc_nfiles, "1")
  expect_equal(final$cac_proc_params, ggir_params_hash())
  expect_match(final$cac_proc_notes, "Source file: 9999/Baseline/Accelerometer/x.bin (850 MB)",
    fixed = TRUE
  )
  expect_equal(final$accelerometer_sleep_complete, "2")
  expect_equal(final$participant_reports_complete, "2")
  expect_false("accelerometer_details_complete" %in% names(final))

  uploads <- Filter(function(x) x$type == "file", calls$log)
  fields <- vapply(uploads, function(x) x$value$field, "")
  expect_equal(fields, c("acc_pt2", "acc_pt4", "acc_pt5", "acc_pt6",
    "reports_accel"
  ))
  expect_equal(uploads[[5]]$value$event, "baseline_visit_2_arm_1")
})

test_that("kv_write() marks results with notes to check", {
  calls <- local_fake_redcap()
  work_dir <- withr::local_tempdir()

  jobs <- suppressMessages(kv_write(
    reported_job(work_dir, check_notes = TRUE), config_for(work_dir)
  ))

  expect_equal(jobs$redcap_status, 2L)
})

test_that("kv_write() leaves a recording in progress if writing fails", {
  calls <- local_fake_redcap()
  local_mocked_bindings(rc_import_file = function(...) stop("timeout"))
  work_dir <- withr::local_tempdir()

  jobs <- suppressMessages(
    kv_write(reported_job(work_dir), config_for(work_dir))
  )

  expect_equal(jobs$status, "write_failed")
  expect_match(jobs$message, "Writing to REDCap failed: timeout")
  imports <- Filter(function(x) x$type == "records", calls$log)
  expect_equal(imports[[length(imports)]]$value$cac_sum_mvpa, "33")
})

test_that("kv_write() imports nothing if REDCap would reject a value", {
  calls <- local_fake_redcap()
  local_mocked_bindings(rc_validate = function(rcon, data) {
    stop("REDCap would not accept cac_sum_mvpa.")
  })
  work_dir <- withr::local_tempdir()

  jobs <- suppressMessages(
    kv_write(reported_job(work_dir), config_for(work_dir))
  )

  expect_equal(jobs$status, "write_failed")
  expect_length(calls$log, 0)
})

test_that("kv_write() records why a recording couldn't be processed", {
  calls <- local_fake_redcap()
  jobs <- fake_jobs(c("1001", "1002", "1003"))
  jobs$status <- c("no_files", "needs_review", "pending")
  jobs$message <- c("No .bin files.", "Files overlap.", NA)

  jobs <- suppressMessages(kv_write(jobs, config_for(withr::local_tempdir())))

  expect_equal(jobs$status, c("no_files", "needs_review", "pending"))
  expect_equal(jobs$redcap_status, c(3L, 6L, NA))
  imports <- Filter(function(x) x$type == "records", calls$log)
  expect_length(imports, 2)
  expect_equal(imports[[1]]$value$cac_proc_status, "3")
  expect_equal(imports[[1]]$value$cac_proc_notes, "No .bin files.")
  expect_equal(imports[[2]]$value$cac_proc_nfiles, "0")
})

test_that("kv_write() removes earlier results that no longer match", {
  calls <- local_fake_redcap()
  jobs <- fake_jobs(c("1001", "1002", "1003"))
  jobs$status <- c("needs_review", "copy_failed", "no_folder")
  jobs$has_results <- TRUE
  jobs$uploaded_files <- list(c("acc_pt2", "reports_accel"), "acc_pt2", "x")

  jobs <- suppressMessages(kv_write(jobs, config_for(withr::local_tempdir())))

  expect_equal(jobs$redcap_status, c(6L, 5L, 3L))
  types <- vapply(calls$log, `[[`, "", "type")
  expect_equal(types, c(
    # 1001: everything checked, then in progress, values blanked, files
    # deleted, then the status
    "validate", "validate", "records", "records", "delete", "delete",
    "records",
    # 1002 and 1003: kept, as they are processed again automatically
    "validate", "records", "validate", "records"
  ))
  imports <- Filter(function(x) x$type == "records", calls$log)
  expect_equal(imports[[1]]$value$cac_proc_status, "0")
  cleared <- imports[[2]]$value
  expect_true(is.na(cleared$cac_sum_mvpa))
  expect_true(is.na(cleared$cac_day1_noc))
  expect_equal(cleared$accelerometer_sleep_complete, "0")
  expect_false(any(c("cac_gen_wrist", "cac_gen_num") %in% names(cleared)))
  expect_equal(imports[[3]]$value$cac_proc_status, "6")
  expect_equal(imports[[3]]$value$cac_proc_params, ggir_params_hash())
  deleted <- Filter(function(x) x$type == "delete", calls$log)
  expect_equal(
    vapply(deleted, function(x) x$value$field, ""),
    c("acc_pt2", "reports_accel")
  )

  # Kept results keep the versions and settings that produced them
  provenance <- c(
    "cac_proc_ggir", "cac_proc_pkg", "cac_proc_params", "cac_proc_nfiles"
  )
  for (kept in imports[4:5]) {
    expect_false(any(provenance %in% names(kept$value)))
    expect_true("cac_proc_notes" %in% names(kept$value))
  }
  expect_equal(imports[[4]]$value$cac_proc_status, "5")
})

test_that("kv_write() removes nothing if REDCap would reject the blanks", {
  calls <- local_fake_redcap()
  local_mocked_bindings(rc_validate = function(rcon, data) {
    if ("cac_sum_mvpa" %in% names(data)) stop("bad blank")
  })
  jobs <- fake_jobs("1001")
  jobs$status <- "needs_review"
  jobs$has_results <- TRUE
  jobs$uploaded_files <- list("acc_pt2")

  jobs <- suppressMessages(kv_write(jobs, config_for(withr::local_tempdir())))

  expect_equal(jobs$status, "write_failed")
  expect_length(calls$log, 0)
})

test_that("kv_write() reports a status it couldn't record", {
  calls <- local_fake_redcap()
  local_mocked_bindings(rc_import_records = function(rcon, data) {
    stop("timeout")
  })
  jobs <- fake_jobs("1001")
  jobs$status <- "no_folder"
  jobs$message <- "No participant folder."

  jobs <- suppressMessages(kv_write(jobs, config_for(withr::local_tempdir())))

  expect_equal(jobs$status, "write_failed")
  expect_true(is.na(jobs$redcap_status))
  expect_equal(jobs$message, "Writing to REDCap failed: timeout")
  expect_equal(
    jobs$notes[[1]], "Not processed (no folder): No participant folder."
  )
})

test_that("kv_write() saves a dry run of removing earlier results", {
  calls <- local_fake_redcap()
  work_dir <- withr::local_tempdir()
  jobs <- fake_jobs("1001")
  jobs$status <- "invalid_file"
  jobs$has_results <- TRUE
  jobs$uploaded_files <- list(c("acc_pt2", "acc_pt4"))

  suppressMessages(kv_write(jobs, config_for(work_dir), dry_run = TRUE))

  dir <- file.path(work_dir, "dry_run", "1001_baseline")
  expect_setequal(basename(fs::dir_ls(dir)), c(
    "1_status.csv", "2_cleared.csv", "3_deleted_files.txt", "4_status.csv"
  ))
  expect_equal(
    readLines(file.path(dir, "3_deleted_files.txt")), c("acc_pt2", "acc_pt4")
  )
})

test_that("kv_write() saves a dry run instead of writing", {
  calls <- local_fake_redcap()
  work_dir <- withr::local_tempdir()

  jobs <- suppressMessages(kv_write(
    reported_job(work_dir), config_for(work_dir), dry_run = TRUE
  ))

  expect_equal(jobs$status, "written")
  types <- vapply(calls$log, `[[`, "", "type")
  expect_equal(types, c("validate", "validate"))
  saved <- sort(basename(fs::dir_ls(
    file.path(work_dir, "dry_run", "9999_baseline")
  )))
  expect_equal(saved, sort(c(
    "1_status.csv", "2_values.csv", "3_status.csv",
    "acc_pt2_9999_baseline_part2.csv", "acc_pt4_9999_baseline_part4.csv",
    "acc_pt5_9999_baseline_part5.csv", "acc_pt6_9999_baseline_part6.csv",
    "reports_accel_KidVision_9999_baseline.pdf"
  )))
})

test_that("rc_validate() stops when REDCap would drop a value", {
  local_mocked_bindings(
    importRecords = function(rcon, data, ...) {
      message("Value(s) must be numeric.")
      data$cac_sum_mvpa <- NA
      data
    },
    .package = "redcapAPI"
  )
  data <- data.frame(record_id = "1", cac_sum_mvpa = "abc", cac_sum_noc = NA)

  expect_error(
    rc_validate("rcon", data),
    "would not accept cac_sum_mvpa. Value\\(s\\) must be numeric."
  )
})

test_that("rc_validate() accepts values REDCap keeps", {
  local_mocked_bindings(
    importRecords = function(rcon, data, ...) data,
    .package = "redcapAPI"
  )
  data <- data.frame(record_id = "1", cac_sum_mvpa = "33", cac_sum_noc = NA)

  expect_no_error(rc_validate("rcon", data))
})

test_that("rc_import_records() sends small numbers as plain decimals", {
  scipen <- getOption("scipen")
  sent <- NULL
  local_mocked_bindings(
    importRecords = function(rcon, data, ...) {
      # As redcapAPI does for number fields
      data$cac_gen_caliberror <- as.numeric(data$cac_gen_caliberror)
      sent <<- utils::capture.output(
        utils::write.csv(data, row.names = FALSE, na = "")
      )
      "1"
    },
    .package = "redcapAPI"
  )

  rc_import_records(
    "rcon", data.frame(record_id = "1", cac_gen_caliberror = "0.0004")
  )

  expect_equal(sent[[2]], '"1",0.0004')
  expect_equal(getOption("scipen"), scipen)
})

test_that("rc_import_records() checks REDCap imported every record", {
  local_mocked_bindings(
    importRecords = function(rcon, data, ...) "1",
    .package = "redcapAPI"
  )

  expect_no_error(rc_import_records("rcon", data.frame(record_id = "1")))
  expect_error(
    rc_import_records("rcon", data.frame(record_id = c("1", "2"))),
    "imported 1 of 2"
  )
})
