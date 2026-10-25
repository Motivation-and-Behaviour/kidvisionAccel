test_that("kv_config() reads settings from environment variables", {
  withr::local_envvar(
    REDCAP_URI = "https://example.org/api/",
    REDCAP_KEY = "secret",
    KIDVISION_SOURCE_DIR = "/onedrive",
    KIDVISION_WORK_DIR = "/work",
    KIDVISION_TZ = "Australia/Brisbane",
    KIDVISION_WORKERS = "4"
  )
  config <- kv_config()

  expect_s3_class(config, "kv_config")
  expect_equal(config$redcap_uri, "https://example.org/api/")
  expect_equal(config$redcap_key, "secret")
  expect_equal(config$source_dir, "/onedrive")
  expect_equal(config$work_dir, "/work")
  expect_equal(config$tz, "Australia/Brisbane")
  expect_identical(config$workers, 4L)
})

test_that("kv_config() tidies the folder paths", {
  config <- kv_config(source_dir = "/onedrive/", work_dir = "~/work//kv")

  expect_equal(config$source_dir, "/onedrive")
  expect_equal(config$work_dir, file.path(fs::path_home(), "work", "kv"))
  expect_equal(kv_config(source_dir = "")$source_dir, "")
})

test_that("kv_config() uses defaults for unset or empty variables", {
  withr::local_envvar(
    REDCAP_URI = "",
    KIDVISION_TZ = NA,
    KIDVISION_WORKERS = NA
  )
  config <- kv_config()

  expect_equal(config$redcap_uri, "https://redcap.acu.edu.au/api/")
  expect_equal(config$tz, "Australia/Sydney")
  expect_identical(config$workers, 1L)
})

test_that("kv_config() rejects invalid worker counts", {
  expect_error(kv_config(workers = 0), "positive whole number")
  expect_error(kv_config(workers = 2.5), "positive whole number")
  expect_error(kv_config(workers = "many"), "positive whole number")
})

test_that("printing a config never shows the REDCap key", {
  config <- kv_config(redcap_key = "supersecret")
  out <- utils::capture.output(print(config))

  expect_false(any(grepl("supersecret", out)))
  expect_true(any(grepl("REDCap key: <set>", out, fixed = TRUE)))
})
