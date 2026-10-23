# REDCap export rows, as rc_export() returns them (everything as strings).
fake_records <- function(...) {
  rows <- list(...)
  columns <- c(
    "record_id", "redcap_event_name", pending_fields[-1],
    "sop2_postchecklist___1"
  )
  columns <- setdiff(columns, "sop2_postchecklist")
  out <- as.data.frame(
    matrix(NA_character_, nrow = length(rows), ncol = length(columns),
      dimnames = list(NULL, columns)
    )
  )
  for (i in seq_along(rows)) {
    for (field in names(rows[[i]])) out[i, field] <- rows[[i]][[field]]
  }
  out
}

# Rows for each event, with typical values that `...` can override.
enrolled <- function(id, ...) {
  utils::modifyList(list(
    record_id = id, redcap_event_name = enrolment_event,
    consent_geneactiv = "1"
  ), list(...))
}

visit1 <- function(id, tp = "baseline", ...) {
  event <- timepoint_map$visit1_event[timepoint_map$timepoint == tp]
  utils::modifyList(list(
    record_id = id, redcap_event_name = event, sop_date = "2025-03-10"
  ), list(...))
}

visit2 <- function(id, tp = "baseline", ...) {
  event <- timepoint_map$visit2_event[timepoint_map$timepoint == tp]
  utils::modifyList(list(
    record_id = id, redcap_event_name = event, sop2_date = "2025-03-18",
    sop2_postchecklist___1 = "1"
  ), list(...), keep.null = TRUE)
}

all_files <- stats::setNames(as.list(paste0(file_fields, ".csv")), file_fields)

test_that("recording_states() works out each recording's state", {
  records <- fake_records(
    enrolled("1"), visit1("1"), visit2("1"),
    enrolled("2"), visit2("2", cac_proc_status = "1"),
    enrolled("3"), do.call(visit2, c(list("3"), all_files)),
    enrolled("4"), visit2("4", cac_proc_status = "0"),
    enrolled("5"), visit2("5", cac_proc_status = "5"),
    enrolled("6"), visit2("6", cac_proc_status = "3"),
    enrolled("7"), visit2("7", cac_proc_status = "6"),
    enrolled("8"), visit2("8", sop2_date = NA),
    enrolled("9"), visit2("9", cac_proc_status = "7")
  )
  states <- recording_states(records)

  expect_equal(states$record_id, as.character(1:9))
  expect_equal(states$state, c(
    "new", "complete", "complete", "in progress", "failed", "no file",
    "needs review", "not yet visited", "cannot be processed"
  ))
  expect_equal(states$visit1_date[1], as.Date("2025-03-10"))
  expect_equal(states$visit2_date[1], as.Date("2025-03-18"))
  expect_true(states$accel_downloaded[1])
})

test_that("recording_states() excludes recordings that must not be used", {
  records <- fake_records(
    enrolled("1", consent_geneactiv = "0"), visit2("1"),
    enrolled("2", withdrawn_existingdata = "2"), visit2("2"),
    enrolled("3", withdrawn_existingdata = "1"), visit2("3"),
    enrolled("4"), visit2("4"),
    visit1("4", "12m", dropout_existingdata = "2"), visit2("4", "12m")
  )
  states <- recording_states(records)

  expect_equal(states$state, c(
    "no consent", "data removal requested", "new",
    "data removal requested", "data removal requested"
  ))
})

test_that("completed recordings are outdated after a GGIR or settings change", {
  current <- list(
    cac_proc_status = "1", cac_proc_ggir = pinned_versions[["GGIR"]],
    cac_proc_params = ggir_params_hash()
  )
  records <- fake_records(
    do.call(visit2, c(list("1"), current)),
    do.call(visit2, c(list("2"), utils::modifyList(
      current, list(cac_proc_ggir = "3.1-4")
    ))),
    do.call(visit2, c(list("3"), utils::modifyList(
      current, list(cac_proc_params = "old")
    ))),
    do.call(visit2, c(list("4"), all_files))
  )

  expect_equal(recording_states(records)$outdated, c(FALSE, TRUE, TRUE, TRUE))
})

test_that("versions match however they're written", {
  expect_equal(
    same_version(c("3.3.9", "3.3-9", "3.1-4", NA, "unknown"), "3.3-9"),
    c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("select_recordings() applies the reprocess modes", {
  recordings <- tibble::tibble(
    record_id = as.character(1:9),
    timepoint = "baseline",
    state = c(
      "new", "in progress", "failed", "no file", "no file", "needs review",
      "complete", "complete", "no consent"
    ),
    has_new_file = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
      FALSE),
    outdated = c(rep(FALSE, 6), TRUE, FALSE, FALSE)
  )
  selected <- function(...) {
    recordings$record_id[select_recordings(recordings, ...)]
  }

  expect_equal(selected("none", NULL, NULL), c("1", "2", "3", "4"))
  expect_equal(selected("outdated", NULL, NULL), c("1", "2", "3", "4", "7"))
  expect_equal(selected("all", NULL, NULL), as.character(1:8))
  expect_equal(selected("none", c("6", "8"), NULL), "6")
  expect_equal(selected("all", "9", NULL), character())
})

test_that("select_recordings() filters by timepoint", {
  recordings <- tibble::tibble(
    record_id = c("1", "1"), timepoint = c("baseline", "12m"),
    state = "new", has_new_file = FALSE, outdated = FALSE
  )

  expect_equal(
    recordings$timepoint[select_recordings(recordings, "none", NULL, "12m")],
    "12m"
  )
})

test_that("kv_pending() returns a job table of recordings to process", {
  source_dir <- local_source(files = c(
    "6/Baseline/Accelerometer/6__029501_2025-03-20 14-43-54.bin"
  ))
  records <- fake_records(
    enrolled("1"), visit1("1"), visit2("1"),
    enrolled("2"), do.call(visit2, c(list("2"), all_files)),
    enrolled("6"), visit2("6", cac_proc_status = "3"),
    enrolled("7"), visit2("7", cac_proc_status = "3")
  )
  local_mocked_bindings(
    rc_connect = function(config) "rcon",
    rc_export = function(rcon, fields, events, ...) {
      expect_setequal(fields, pending_fields)
      records
    }
  )
  config <- kv_config(source_dir = source_dir)

  jobs <- suppressMessages(kv_pending(config))

  expect_equal(jobs$record_id, c("1", "6"))
  expect_equal(jobs$reason, c("new", "no file"))
  expect_equal(jobs$status, c("pending", "pending"))
  expect_equal(jobs$event, rep("baseline_visit_2_arm_1", 2))
  expect_named(jobs, c(
    "record_id", "timepoint", "event", "reason", "visit1_date",
    "visit2_date", "accel_downloaded", "device_num", "status", "message",
    "notes", "check_notes"
  ))
})

test_that("kv_pending() rejects unknown timepoints", {
  expect_error(kv_pending(timepoints = "36m"), "must be one of")
})
