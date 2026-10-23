test_that("GGIR settings only change deliberately", {
  # If this fails, the processed data will change: bump the package version,
  # add a NEWS.md entry and reprocess with kv_run(reprocess = "outdated").
  expect_snapshot_value(ggir_params(), style = "deparse")
  expect_snapshot(ggir_params_hash())
})

test_that("intensity thresholds are the Hildebrand child wrist values", {
  params <- ggir_params()

  expect_equal(params$threshold.lig, 56.3)
  expect_equal(params$threshold.mod, 191.6)
  expect_equal(params$threshold.vig, 695.8)
})

test_that("the timezone is always set explicitly", {
  params <- ggir_params("Australia/Brisbane")

  expect_equal(params$desiredtz, "Australia/Brisbane")
  expect_equal(params$configtz, "Australia/Brisbane")
})

test_that("part 2 and part 5 use the same valid-day rule", {
  expect_equal(ggir_params()$includedaycrit, c(16, 16))
})

test_that("the settings hash changes when the settings change", {
  expect_equal(ggir_params_hash(), ggir_params_hash())
  expect_false(
    ggir_params_hash("Australia/Sydney") == ggir_params_hash("Australia/Perth")
  )
})

test_that("the installed GGIR recognises every setting", {
  expect_length(ggir_unknown_params(ggir_params()), 0)
  expect_equal(
    ggir_unknown_params(list(mode = 1, not_a_setting = 1)),
    "not_a_setting"
  )
})

test_that("GGIR's own parameter checks accept the settings", {
  expect_no_warning(GGIR:::extract_params(input = ggir_params()))
})
