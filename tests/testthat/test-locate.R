accel <- function(id = "1001", tp = "Baseline", file = bin_name(id),
                  folder = "Accelerometer") {
  file.path(id, tp, folder, file)
}

locate <- function(source_dir, id = "1001", tp = "baseline") {
  recording_files(source_dir, id, tp, tz = "Australia/Sydney")
}

test_that("recording_files() finds a recording's .bin file", {
  source_dir <- local_source(accel())
  found <- locate(source_dir)

  expect_equal(found$status, "located")
  expect_equal(found$folder, file.path(source_dir, "1001/Baseline/Accelerometer"))
  expect_equal(nrow(found$files), 1)
  expect_equal(found$files$subject, "1001")
  expect_equal(found$files$serial, "029501")
  expect_equal(
    found$files$downloaded,
    as.POSIXct("2025-03-20 14:43:54", tz = "Australia/Sydney")
  )
})

test_that("recording_files() ignores the case of folder names", {
  source_dir <- local_source(accel(tp = "time_1", folder = "accelerometer"))

  expect_equal(locate(source_dir, tp = "12m")$status, "located")
})

test_that("recording_files() reports misnamed folders instead of guessing", {
  expect_match(
    locate(local_source(accel(tp = "Time 1")), tp = "12m")$message,
    'Found "Time 1" in 1001; rename to "Time_1"'
  )
  expect_equal(
    locate(local_source(accel(tp = "Time_1 - Copy")), tp = "12m")$status,
    "misnamed"
  )
  expect_match(
    locate(local_source(accel(folder = "Accelerometers")))$message,
    'Found "Accelerometers" in 1001/Baseline; rename to "Accelerometer"'
  )
  expect_match(
    locate(local_source(dirs = "1001(1003)"))$message,
    'Found "1001\\(1003\\)" in the source folder; rename to "1001"'
  )
})

test_that("recording_files() reports missing folders and files", {
  source_dir <- local_source(
    dirs = c("1001/Baseline/Accelerometer", "1002/Baseline/Images", "1003")
  )

  expect_equal(locate(source_dir)$message, "No .bin files.")
  expect_equal(locate(source_dir, "1002")$message, "No Accelerometer folder.")
  expect_equal(locate(source_dir, "1003")$message, "No Baseline folder.")
  expect_equal(locate(source_dir, "1004")$status, "no_folder")
  expect_true(locate(source_dir, "1004")$no_participant_folder)
})

test_that("recording_files() searches subfolders except Excluded", {
  source_dir <- local_source(c(
    accel(file = file.path("Accel 1", bin_name("1001", "035318"))),
    accel(file = file.path("Accel 2", "Accel 2", bin_name("1001", "029385"))),
    accel(file = file.path("Excluded", bin_name("1001", "045999")))
  ))
  found <- locate(source_dir)

  expect_equal(found$status, "located")
  expect_setequal(found$files$serial, c("035318", "029385"))
})

test_that("recording_files() ignores empty files and copies", {
  source_dir <- local_source(
    files = c(
      accel(),
      accel(file = file.path("Accel 1", bin_name("1001")))
    ),
    empty = accel(file = bin_name("1001", "035318"))
  )
  found <- locate(source_dir)

  expect_equal(found$status, "located")
  expect_equal(nrow(found$files), 1)
  expect_match(found$notes, "Ignored empty file", all = FALSE)
  expect_match(found$notes, "a copy of another file", all = FALSE)
})

test_that("recording_files() rejects a folder of empty files", {
  source_dir <- local_source(empty = accel())

  expect_equal(locate(source_dir)$status, "invalid_file")
})

test_that("recording_files() flags files labelled as another participant", {
  found <- locate(local_source(accel(file = bin_name("1288"))))

  expect_equal(found$status, "needs_review")
  expect_match(found$message, 'labelled as participant "1288"')
})

test_that("recording_files() accepts subject codes with suffixes or none", {
  source_dir <- local_source(c(
    accel(file = bin_name("1001.1", "045999")),
    accel(file = bin_name("", "029084"))
  ))

  expect_equal(locate(source_dir)$status, "located")
})

test_that("parse_bin_filename() reads GENEActiv file names", {
  parsed <- parse_bin_filename(
    c("1268.1__045999_2024-07-05 19-22-32.bin", "renamed.bin"),
    tz = "Australia/Sydney"
  )

  expect_equal(parsed$subject, c("1268.1", NA))
  expect_equal(parsed$serial, c("045999", NA))
  expect_equal(
    format(parsed$downloaded),
    c("2024-07-05 19:22:32", NA)
  )
})

test_that("kv_locate() locates pending recordings", {
  source_dir <- local_source(accel())
  jobs <- fake_jobs(c("1001", "1002"))

  jobs <- suppressMessages(kv_locate(jobs, kv_config(source_dir = source_dir)))

  expect_equal(jobs$status, c("located", "no_folder"))
  expect_equal(nrow(jobs$files[[1]]), 1)
  expect_null(jobs$files[[2]])
})

test_that("kv_locate() leaves recordings in other statuses alone", {
  source_dir <- local_source(accel())
  jobs <- fake_jobs("1001")
  jobs$status <- "misnamed"

  located <- suppressMessages(
    kv_locate(jobs, kv_config(source_dir = source_dir))
  )

  expect_equal(located$status, "misnamed")
})

test_that("kv_locate() flags files downloaded before visit 1", {
  source_dir <- local_source(c(
    accel("1001", file = bin_name("1001", downloaded = "2025-03-01 10-00-00")),
    accel("1002", file = bin_name("1002", downloaded = "2025-03-20 10-00-00"))
  ))
  jobs <- fake_jobs(c("1001", "1002"), visit1_date = "2025-03-10")

  jobs <- suppressMessages(kv_locate(jobs, kv_config(source_dir = source_dir)))

  expect_equal(jobs$status, c("needs_review", "located"))
  expect_match(jobs$message[1], "downloaded on 2025-03-01, but visit 1 was")
})

test_that("kv_locate() flags the same file in two recordings", {
  file <- function(id) bin_name(id, "035336", "2026-04-10 12-38-37")
  source_dir <- local_source(c(
    accel("1001", "Time_1", file("1001")),
    accel("1002", "Time_1", file("1002")),
    accel("1003", "Time_1", bin_name("1003", "035336", "2026-05-10 12-00-00"))
  ))
  jobs <- fake_jobs(c("1001", "1002", "1003"), "12m")

  jobs <- suppressMessages(kv_locate(jobs, kv_config(source_dir = source_dir)))

  expect_equal(jobs$status, c("needs_review", "needs_review", "located"))
  expect_match(jobs$message[1], "same file .* as one in 1002 12m")
})

test_that("kv_locate() stops when the source folder looks unmounted", {
  config <- kv_config(source_dir = local_source())

  expect_error(kv_locate(fake_jobs("1001"), config), "No participant folders")
})

test_that("kv_locate() stops when most participant folders are missing", {
  source_dir <- local_source(dirs = "9999")
  jobs <- fake_jobs(as.character(1001:1010))

  expect_error(
    suppressMessages(kv_locate(jobs, kv_config(source_dir = source_dir))),
    "10 of 10 recordings have no participant folder"
  )
})

test_that("kv_audit_folders() lists folders to fix", {
  source_dir <- local_source(
    files = c(
      accel("1001"),
      accel("1002", "Time1"),
      accel("1003", folder = "Accelerometers"),
      accel("1004", file = bin_name("1288"))
    ),
    dirs = c("Practice Data", "0000/Test", "1005/Drop Out", "Misc")
  )

  issues <- suppressMessages(
    kv_audit_folders(kv_config(source_dir = source_dir))
  )

  expect_equal(issues$folder, c(
    "1002/Time1", "1003/Baseline", "1004/Baseline/Accelerometer",
    "1005/Drop Out", "Misc"
  ))
  expect_equal(issues$fix[1:2], c(
    'Rename to "Time_1"', 'Rename to "Accelerometer"'
  ))
  expect_match(issues$problem[3], "labelled as participant")
})
