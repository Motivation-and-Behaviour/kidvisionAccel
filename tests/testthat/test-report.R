test_that("report_data() summarises activity and sleep", {
  data <- report_data(
    fixture_days(), child = "Sam", parent = "Alex",
    dob = as.Date("2008-01-15"), timepoint = "12m"
  )

  expect_equal(data$child_label, "Sam")
  expect_equal(data$child_possessive, "Sam’s")
  expect_equal(data$timepoint_label, "12-month visit")
  expect_equal(data$first_day, "23 June 2016")
  expect_equal(data$last_day, "27 June 2016")
  expect_equal(data$age, 8)

  expect_true(data$activity$available)
  expect_equal(data$activity$days, 3)
  expect_equal(data$activity$days_meeting, 0)
  expect_equal(data$activity$average, "24 min")

  expect_equal(data$sleep$nights, 2)
  expect_equal(data$sleep$guideline, "9 to 11 hours")
  expect_equal(data$sleep$average, "10 h 15 min")
  expect_equal(data$sleep$bedtime, "8:55 pm")
  expect_equal(data$sleep$wake, "7:10 am")
  expect_equal(data$sleep$efficiency, "98%")
})

test_that("report_data() works without names or a date of birth", {
  data <- report_data(fixture_days())

  expect_equal(data$child_label, "Your child")
  expect_equal(data$child_possessive, "Your child’s")
  expect_true(is.na(data$parent))
  expect_true(is.na(data$age))
  expect_equal(data$sleep$guideline, "9 to 11 hours")
})

test_that("report_data() handles recordings without valid days or nights", {
  days <- fixture_days()
  days$valid_day <- FALSE
  days$valid_night <- FALSE
  data <- report_data(days)

  expect_false(data$activity$available)
  expect_false(data$sleep$available)
  expect_true(is.na(data$activity$average))
  expect_true(is.na(data$day$sleep))
})

test_that("average_day() scales each day to 24 hours", {
  days <- tibble::tibble(
    sleep_period_sleep = c(540, 540), sleep_period_awake = c(60, 60),
    sedentary = c(600, 660), light = c(150, 150), mvpa = c(30, 90),
    window = c(1380, 1500)
  )
  day <- average_day(days)

  expect_equal(sum(unlist(day)), 24)
})

test_that("sleep_guideline() follows the Australian guidelines by age", {
  expect_equal(sleep_guideline(4), c(10, 13))
  expect_equal(sleep_guideline(5), c(9, 11))
  expect_equal(sleep_guideline(13), c(9, 11))
  expect_equal(sleep_guideline(14), c(8, 10))
  expect_equal(sleep_guideline(NA), c(9, 11))
})

test_that("age_on() counts whole years", {
  dob <- as.Date("2015-06-10")

  expect_equal(age_on(dob, as.Date("2023-06-09")), 7)
  expect_equal(age_on(dob, as.Date("2023-06-10")), 8)
  expect_true(is.na(age_on(NA, as.Date("2023-06-10"))))
})

test_that("possessive() handles names ending in s", {
  expect_equal(possessive("Chris"), "Chris’")
})

test_that("report formatting is written for families", {
  expect_equal(format_minutes(45.4), "45 min")
  expect_equal(format_minutes(125), "2 h 5 min")
  expect_true(is.na(format_minutes(NaN)))
  expect_equal(format_hours(9.75), "9 h 45 min")
  expect_equal(format_clock(21.25), "9:15 pm")
  expect_equal(format_clock(26.5), "2:30 am")
  expect_equal(format_clock(24), "12:00 am")
  expect_equal(format_clock(36), "12:00 pm")
  expect_equal(short_clock(21), "9 pm")
  expect_equal(axis_clock(24), "midnight")
  expect_equal(axis_clock(36), "noon")
  expect_equal(format_percent(0.796), "80%")
})

test_that("write_report_charts() draws only charts with data", {
  dir <- withr::local_tempdir()
  days <- fixture_days()
  data <- report_data(days)

  expect_setequal(
    write_report_charts(data, dir),
    c("activity", "day", "sleep", "timing", "efficiency")
  )
  expect_true(all(file.exists(file.path(
    dir, c("activity", "day", "sleep", "timing", "efficiency"), fsep = "/"
  ) |> paste0(".png"))))

  days$valid_night <- FALSE
  expect_setequal(
    write_report_charts(report_data(days), withr::local_tempdir()),
    c("activity", "day")
  )
})

skip_if_not(nzchar(Sys.which("quarto")), "Quarto isn't installed")

is_pdf <- function(path) {
  identical(readBin(path, "raw", 4), charToRaw("%PDF"))
}

test_that("render_report() creates a PDF", {
  output <- file.path(withr::local_tempdir(), "sub", "report.pdf")

  render_report(report_data(fixture_days(), child = "Sam"), output)

  expect_true(is_pdf(output))
})

test_that("render_report() copes with names that look like Typst markup", {
  output <- file.path(withr::local_tempdir(), "report.pdf")
  data <- report_data(fixture_days(), child = "#Sam *[x]* $y$ @ref")

  expect_no_error(render_report(data, output))
})

test_that("render_report() creates a report without valid days or nights", {
  days <- fixture_days()
  days$valid_day <- FALSE
  days$valid_night <- FALSE
  output <- file.path(withr::local_tempdir(), "report.pdf")

  render_report(report_data(days), output)

  expect_true(is_pdf(output))
})

collated_job <- function(work_dir) {
  jobs <- fake_jobs("9999")
  jobs$status <- "collated"
  jobs$job_dir <- file.path(work_dir, "jobs", "9999_baseline")
  jobs$days <- list(fixture_days(parent.frame()))
  jobs
}

test_that("kv_report() creates each recording's report", {
  local_mocked_bindings(participant_details = fake_participant_details)
  work_dir <- withr::local_tempdir()

  jobs <- suppressMessages(kv_report(collated_job(work_dir), kv_config()))

  expect_equal(jobs$status, "reported")
  expect_equal(
    jobs$report,
    file.path(jobs$job_dir, "out", "KidVision_9999_baseline.pdf")
  )
  expect_true(is_pdf(jobs$report))
})

test_that("kv_report() marks reports that can't be created", {
  local_mocked_bindings(
    participant_details = function(config, ids) {
      data.frame(record_id = "9999", cid_namef = NA, pid_namef = NA,
        cid_dob = NA
      )
    },
    render_report = function(data, output) stop("Typst failed")
  )

  jobs <- suppressMessages(
    kv_report(collated_job(withr::local_tempdir()), kv_config())
  )

  expect_equal(jobs$status, "report_failed")
  expect_match(jobs$message, "couldn't be created: Typst failed")
})

test_that("kv_report() marks reports when REDCap can't be read", {
  local_mocked_bindings(participant_details = function(config, ids) {
    stop("REDCap timed out")
  })

  jobs <- suppressMessages(
    kv_report(collated_job(withr::local_tempdir()), kv_config())
  )

  expect_equal(jobs$status, "report_failed")
  expect_match(jobs$message, "couldn't be read from REDCap: REDCap timed out")
})

test_that("save_chart() closes its device if the chart fails", {
  devices <- grDevices::dev.list()
  chart <- ggplot2::ggplot(data.frame(x = 1), ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_point()

  expect_error(
    save_chart(chart, file.path(withr::local_tempdir(), "x.png"), 1)
  )
  expect_identical(grDevices::dev.list(), devices)
})

test_that("crowded charts get compact labels without rounding to :60", {
  table <- tibble::tibble(
    date = as.Date("2025-05-27") + 0:11,
    period = c(9.9999, rep(10.5, 11)), asleep = 9,
    efficiency = 0.9, onset = 22, wake = 31
  )
  plot <- chart_sleep(table, c(9, 11))
  labels <- ggplot2::layer_data(plot, 3)$label

  expect_equal(labels[1], "10:00")
  expect_equal(labels[2], "10:30")
})
