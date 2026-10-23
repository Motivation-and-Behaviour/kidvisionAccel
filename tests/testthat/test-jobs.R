test_that("report_jobs() lists problems only for the step's recordings", {
  jobs <- fake_jobs(c("1001", "1002", "1003"))
  jobs$status <- c("needs_review", "ready", "copy_failed")
  jobs$message <- c("old problem", NA, "new problem")

  expect_message(
    out <- cli::cli_fmt(report_jobs(jobs, "Fetch", rows = 2:3)),
    "Fetch: 1 ready, 1 needs review, 1 copy failed."
  )
  expect_true(any(grepl("1003 baseline (copy failed): new problem", out,
    fixed = TRUE
  )))
  expect_false(any(grepl("old problem", out)))
})

test_that("report_jobs() prints messages literally", {
  jobs <- fake_jobs("1001")
  jobs$status <- "needs_review"
  jobs$message <- "A {brace} in a file name"

  out <- suppressMessages(cli::cli_fmt(report_jobs(jobs, "Locate")))

  expect_true(any(grepl("A {brace} in a file name", out, fixed = TRUE)))
})
