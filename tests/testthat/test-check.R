fake_metadata <- function(drop = character()) {
  data.frame(field_name = setdiff(c("record_id", redcap_fields()$field), drop))
}

fake_events <- function() {
  data.frame(unique_event_name = c(
    enrolment_event, timepoint_map$visit1_event, timepoint_map$visit2_event
  ))
}

fake_export <- function(rcon, fields, events = NULL, records = NULL) {
  if (identical(fields, "record_id")) {
    data.frame(record_id = c("1001", "1002"))
  } else {
    data.frame(record_id = c("1001", "1002"), cid_namef = c("Alex", "Sam"))
  }
}

# Mocks everything outside the package, so kv_check() passes.
local_passing_environment <- function(env = parent.frame()) {
  local_mocked_bindings(
    installed_version = function(pkg) pinned_versions[[pkg]],
    check_quarto = function() check_result("Quarto", "pass", "Quarto 1.10"),
    disk_free_gb = function(path) 100,
    rc_connect = function(config) "rcon",
    rc_project_info = function(rcon) {
      data.frame(
        project_id = 1694, project_title = "KidVision", in_production = 1
      )
    },
    rc_metadata = function(rcon) fake_metadata(),
    rc_events = function(rcon) fake_events(),
    rc_export = fake_export,
    .env = env
  )
}

local_test_config <- function(env = parent.frame()) {
  source_dir <- withr::local_tempdir(.local_envir = env)
  fs::dir_create(file.path(source_dir, c("1001", "1002")))
  kv_config(
    redcap_uri = "https://example.org/api/",
    redcap_key = "key",
    source_dir = source_dir,
    work_dir = withr::local_tempdir(.local_envir = env)
  )
}

test_that("kv_check() passes when everything is in place", {
  local_passing_environment()
  config <- local_test_config()

  results <- suppressMessages(kv_check(config))

  expect_true(all(results$status == "pass"))
  expect_true(all(c(
    "GGIR version", "GGIRread version", "GGIR settings", "Timezone",
    "Source folder", "Work folder", "Quarto", "REDCap connection",
    "REDCap fields", "REDCap status fields", "REDCap events",
    "REDCap identifiers"
  ) %in% results$check))
})

test_that("kv_check() aborts on a failure unless error = FALSE", {
  local_passing_environment()
  local_mocked_bindings(
    rc_project_info = function(rcon) stop("403 Forbidden")
  )
  config <- local_test_config()

  expect_error(suppressMessages(kv_check(config)), "1 check failed")
  results <- suppressMessages(kv_check(config, error = FALSE))
  failed <- results[results$status == "fail", ]
  expect_equal(failed$check, "REDCap connection")
  expect_match(failed$message, "Could not connect.*403 Forbidden")
})

test_that("safely_check() turns unexpected errors into failures", {
  result <- safely_check("Something", stop("boom"))

  expect_equal(result$status, "fail")
  expect_equal(result$message, "boom")
})

test_that("check_versions() requires the exact pinned versions", {
  local_mocked_bindings(installed_version = function(pkg) {
    if (pkg == "GGIR") "3.3.8" else pinned_versions[[pkg]]
  })
  results <- check_versions()

  expect_equal(results$status, c("fail", "pass"))
  expect_match(results$message[1], "3.3.8 is installed, but 3.3-9 is required")
  expect_match(results$message[1], "cran/GGIR@3.3-9", fixed = TRUE)
})

test_that("check_versions() reports packages that are not installed", {
  local_mocked_bindings(installed_version = function(pkg) NA_character_)

  expect_match(check_versions()$message, "not installed")
})

test_that("check_ggir_params() reports settings GGIR no longer accepts", {
  local_mocked_bindings(ggir_unknown_params = function(params) "old_setting")
  result <- check_ggir_params("Australia/Sydney")

  expect_equal(result$status, "fail")
  expect_match(result$message, "old_setting")
})

test_that("check_timezone() rejects invalid names", {
  expect_equal(check_timezone("Australia/Sydney")$status, "pass")
  expect_equal(check_timezone("Sydney")$status, "fail")
})

test_that("check_source_dir() catches a missing or empty mount", {
  root <- withr::local_tempdir()

  expect_match(check_source_dir("")$message, "Not set")
  expect_match(check_source_dir(file.path(root, "nope"))$message, "not exist")
  expect_match(check_source_dir(root)$message, "No participant folders")

  fs::dir_create(file.path(root, c("1353", "1148(1146)", "Practice Data")))
  result <- check_source_dir(root)
  expect_equal(result$status, "pass")
  expect_match(result$message, "^2 participant folders")
})

test_that("check_work_dir() needs a writable folder with enough space", {
  dir <- withr::local_tempdir()

  expect_match(check_work_dir("", 20)$message, "Not set")
  expect_match(check_work_dir(file.path(dir, "nope"), 20)$message, "not exist")

  local_mocked_bindings(disk_free_gb = function(path) 100)
  expect_equal(check_work_dir(dir, 20)$status, "pass")

  local_mocked_bindings(disk_free_gb = function(path) 5)
  expect_match(check_work_dir(dir, 20)$message, "Only 5 GB free")
})

test_that("check_redcap() needs credentials", {
  config <- kv_config(redcap_uri = "https://example.org/api/", redcap_key = "")

  result <- check_redcap(config, dry_run = FALSE)

  expect_match(result$message, "must both be set")
})

test_that("check_redcap_fields() reports missing fields", {
  results <- check_redcap_fields(
    fake_metadata(drop = "cac_day3_eff"),
    dry_run = FALSE
  )

  expect_equal(results$status, c("fail", "pass"))
  expect_match(results$message[1], "cac_day3_eff")
})

test_that("missing status fields fail live runs but only warn dry runs", {
  metadata <- fake_metadata(drop = status_fields)

  expect_equal(
    check_redcap_fields(metadata, dry_run = FALSE)$status, c("pass", "fail")
  )
  expect_equal(
    check_redcap_fields(metadata, dry_run = TRUE)$status, c("pass", "warn")
  )
})

test_that("check_redcap_events() reports missing events", {
  events <- fake_events()[-2, , drop = FALSE]
  result <- check_redcap_events(events)

  expect_equal(result$status, "fail")
  expect_match(result$message, "baseline_visit_1_arm_1")
})

test_that("check_redcap_identifiers() detects de-identified export rights", {
  local_mocked_bindings(rc_export = function(rcon, fields, ...) {
    data.frame(record_id = "1001")
  })

  expect_equal(check_redcap_identifiers("rcon")$status, "fail")
})

test_that("check_redcap_identifiers() warns when names are all blank", {
  local_mocked_bindings(rc_export = function(rcon, fields, ...) {
    data.frame(record_id = "1001", cid_namef = NA_character_)
  })

  expect_equal(check_redcap_identifiers("rcon")$status, "warn")
})

test_that("collapse_names() truncates long lists", {
  expect_equal(collapse_names(c("a", "b")), "a, b")
  expect_equal(collapse_names(letters, max = 2), "a, b, and 24 more")
})
