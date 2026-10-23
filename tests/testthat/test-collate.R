fixture_results <- function(edit = NULL, env = parent.frame()) {
  read_ggir_results(local_ggir_output(edit, env = env))
}

test_that("read_ggir_results() reads every result file", {
  results <- fixture_results()

  expect_named(results, names(ggir_results_files))
  expect_equal(nrow(results$part2_day), 5)
  expect_equal(nrow(results$part5_mm), 3)
})

test_that("read_ggir_results() reports missing files", {
  ggir_dir <- local_ggir_output(function(results) {
    file.remove(file.path(results, "part6_summary.csv"))
  })

  expect_error(read_ggir_results(ggir_dir), "missing part6_summary.csv")
})

test_that("day_table() builds one row per day from parts 2, 4 and 5", {
  days <- day_table(fixture_results())

  expect_equal(days$day, 1:5)
  expect_equal(days$date, as.Date("2016-06-23") + 0:4)
  expect_equal(days$weekday, c(4, 5, 6, 7, 1))
  expect_equal(days$weekend, c(FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(days$valid_hours, c(0, 22.25, 22.25, 22.25, 0))
  expect_equal(days$valid_day, c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_equal(days$moderate[2:4], c(24, 24, 24))
  expect_equal(days$mvpa, days$moderate + days$vigorous)
  expect_equal(days$window[2], 1440)
  expect_equal(
    days$sleep_period_sleep[2] + days$sleep_period_awake[2] +
      days$sedentary[2] + days$light[2] + days$mvpa[2],
    1440,
    tolerance = 0.01
  )
  # Each night belongs to the day whose evening it starts on
  expect_equal(days$valid_night, c(FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(days$onset_time[2], "20:55:10")
  expect_equal(days$sleep_efficiency[2], 10.042 / 10.249)
})

test_that("day_table() starts at the first study date", {
  days <- day_table(
    fixture_results(), as.Date("2016-06-24"), as.Date("2016-06-26")
  )

  expect_equal(days$day, 1:3)
  expect_equal(days$date, as.Date("2016-06-24") + 0:2)
})

test_that("to_redcap_record() fills every derived field", {
  results <- fixture_results()
  record <- to_redcap_record(day_table(results), results, "9999", "event")
  derived <- redcap_fields()$field[redcap_fields()$role == "derived"]

  expect_equal(nrow(record), 1)
  expect_setequal(names(record), c("record_id", "redcap_event_name", derived))
  expect_true(all(vapply(record, is.character, logical(1))))
})

test_that("to_redcap_record() gives day values for valid days only", {
  results <- fixture_results()
  record <- to_redcap_record(day_table(results), results, "9999", "event")

  expect_equal(record$cac_day1_calday, "2016-06-23")
  expect_equal(record$cac_day1_dotw, "4")
  expect_equal(record$cac_day1_vhours, "0")
  expect_true(is.na(record$cac_day1_full_mvpa))
  expect_equal(record$cac_day2_full_mpa, "24")
  expect_equal(record$cac_day2_full_lpa, "88.2")
  expect_equal(record$cac_day2_noc, "10.04")
  expect_equal(record$cac_day2_onsetts, "20:55:10")
  expect_equal(record$cac_day2_wake, "31.17")
  expect_equal(record$cac_day2_eff, "0.98")
  expect_true(is.na(record$cac_day4_noc))
  # Days after the recording are blank, so stale values are cleared
  expect_true(is.na(record$cac_day6_calday))
  expect_true(is.na(record$cac_day10_full_sed))
})

test_that("to_redcap_record() averages valid days and nights", {
  results <- fixture_results()
  record <- to_redcap_record(day_table(results), results, "9999", "event")

  expect_equal(record$cac_sum_mpa, "24")
  # Friday is the only valid weekday; Saturday and Sunday are the weekend
  expect_equal(record$cac_sum_lpawd, "88.2")
  expect_equal(record$cac_sum_lpawe, "88.3")
  expect_equal(record$cac_sum_noc, "10.04")
  expect_equal(record$cac_gen_validwd, "1")
  expect_equal(record$cac_gen_validwe, "2")
  expect_equal(record$cac_sum_enmo, "34.28")
  expect_equal(record$cac_gen_protodays, "3")
  expect_equal(record$cac_gen_caliberror, "0.003")
  expect_equal(record$cac_gen_nights, "2")
  expect_equal(record$cac_gen_sleepdef, "T5A5")
})

test_that("format_number() never uses scientific notation", {
  expect_equal(
    format_number(c(0.00001, 24, 88.16666, NA, 1e6), 2),
    c("0", "24", "88.17", NA, "1000000")
  )
})

processed_job <- function(ggir_dir, work_dir) {
  jobs <- fake_jobs("9999")
  jobs$status <- "processed"
  jobs$job_dir <- file.path(work_dir, "jobs", "9999_baseline")
  jobs$ggir_dir <- ggir_dir
  jobs$study_first <- as.Date(NA)
  jobs$study_last <- as.Date(NA)
  jobs
}

test_that("kv_collate() writes the part files and REDCap values", {
  work_dir <- withr::local_tempdir()
  jobs <- processed_job(local_ggir_output(), work_dir)

  jobs <- suppressMessages(kv_collate(jobs, kv_config(work_dir = work_dir)))

  expect_equal(jobs$status, "collated")
  files <- jobs$part_files[[1]]
  expect_named(files, c("part2", "part4", "part5", "part6"))
  expect_equal(
    basename(files), paste0("9999_baseline_", names(files), ".csv")
  )

  part5 <- read_ggir_csv(files[["part5"]])
  expect_equal(part5$window, c("MM", "MM", "MM", "WW", "WW"))
  expect_equal(unique(part5$record_id), "9999")
  expect_equal(unique(part5$redcap_event_name), "baseline_visit_2_arm_1")
  expect_equal(unique(part5$ggir_version), "3.3.9")
  expect_false(anyNA(part5$kidvisionaccel_version))

  expect_equal(jobs$redcap[[1]]$cac_day2_full_mpa, "24")
  saved <- read_ggir_csv(file.path(
    jobs$job_dir, "out", "9999_baseline_redcap.csv"
  ))
  expect_equal(saved$cac_day2_full_mpa, "24")
  expect_equal(nrow(jobs$days[[1]]), 5)
})

test_that("kv_collate() notes days beyond REDCap's 10", {
  ggir_dir <- local_ggir_output(function(results) {
    path <- file.path(results, "part2_daysummary.csv")
    day <- read_ggir_csv(path)[2, ]
    days <- day[rep(1, 12), ]
    days$calendar_date <- format(as.Date("2016-06-23") + 0:11)
    utils::write.csv(days, path, row.names = FALSE)
  })
  work_dir <- withr::local_tempdir()

  jobs <- suppressMessages(kv_collate(
    processed_job(ggir_dir, work_dir), kv_config(work_dir = work_dir)
  ))

  expect_equal(jobs$status, "collated")
  expect_match(jobs$notes[[1]], "covers 12 days; only the first 10")
  expect_equal(jobs$redcap[[1]]$cac_day10_calday, "2016-07-02")
})

test_that("kv_collate() flags recordings with no valid day or night", {
  ggir_dir <- local_ggir_output(function(results) {
    empty_result(results, "part5_daysummary_MM_*.csv")
    empty_result(results, "part4_nightsummary_sleep_cleaned.csv")
  })
  work_dir <- withr::local_tempdir()

  jobs <- suppressMessages(kv_collate(
    processed_job(ggir_dir, work_dir), kv_config(work_dir = work_dir)
  ))

  expect_equal(jobs$status, "no_valid_days")
})

test_that("kv_collate() marks recordings whose results can't be read", {
  ggir_dir <- local_ggir_output(function(results) {
    file.remove(file.path(results, "part6_summary.csv"))
  })
  work_dir <- withr::local_tempdir()

  jobs <- suppressMessages(kv_collate(
    processed_job(ggir_dir, work_dir), kv_config(work_dir = work_dir)
  ))

  expect_equal(jobs$status, "ggir_failed")
  expect_match(jobs$message, "couldn't be merged: missing part6_summary.csv")
})
