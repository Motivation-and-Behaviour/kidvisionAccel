# kidvisionAccel 0.1.0

The package is now a complete pipeline, from REDCap to REDCap: `kv_run()`
finds the recordings that need processing, copies their files from
OneDrive, runs a pinned version of GGIR, writes the results, files and a
participant report to REDCap, and deletes the local copies. See
`vignette("kidvisionAccel")`.

## Breaking changes

* `process_file()`, `generate_report()` and `upload_report()` have been
  removed, replaced by `kv_run()` and the step functions below.
  `upload_report()` also used REDCap event names that don't exist for the
  12- and 24-month visits.

## New features

* `kv_config()` collects credentials, paths and options from environment
  variables.
* `kv_check()` checks everything a run needs before it starts: the pinned
  package versions, GGIR settings, timezone, source and work folders, Quarto,
  and the REDCap connection, fields, events and identifier access.
* `ggir_params()` is the single, snapshot-tested source of GGIR settings, and
  `ggir_params_hash()` identifies them.
* `kv_pending()` reads REDCap to find recordings that need processing (step
  1). It skips recordings without consent or where data removal was
  requested, and can select recordings to reprocess after a GGIR or settings
  change.
* `kv_locate()` finds each recording's `.bin` files on OneDrive from folder
  listings and file names only. It reports misnamed folders, copies of the
  same file, files labelled as another participant, and files downloaded
  before visit 1, and says when a missing file was recorded as downloaded at
  visit 2 (step 2).
* `kv_fetch()` copies the files to the work folder with retries and checks
  them before processing: size, completeness and the device's timezone.
  Files from a device swap are appended. Devices started within 24 hours of
  each other, or more than 3 days after the previous one stopped, need
  review (step 2).
* `kv_audit_folders()` lists OneDrive folders to rename or fix.
* `kv_process()` runs GGIR on each recording in its own R process, with a
  timeout and a log, and judges success from GGIR's output (step 3). When the
  REDCap visit dates are plausible, GGIR uses only the data from visit 1 to
  visit 2.
* `kv_collate()` writes one CSV per GGIR part (2, 4, 5 and 6) for REDCap,
  builds a table of measurement days, and prepares the values for REDCap's
  accelerometer instruments (step 4).
* `kv_report()` creates a PDF report for each child and their family (step
  5), replacing the R Markdown and pagedown report. It is a Typst template
  (`inst/report/report.typ`) compiled by the Typst bundled with Quarto, so
  Chrome is no longer needed. The report:
  * shows dates rather than weekdays, so recordings longer than a week are
    shown correctly (#6);
  * adds an at-a-glance page, an average 24-hour day and a sleep timing
    chart;
  * compares each night's sleep period with the age-appropriate guideline,
    showing how much of it was scored as asleep;
  * no longer calls 80% sleep efficiency the target, as wrist-worn monitors
    score restless sleep as wake.
* `kv_write()` writes the results, the part files and the report to REDCap
  (step 6), and records a processing status for every recording, including
  those that couldn't be processed. Values are checked against the project
  first, so a value REDCap would reject stops the recording instead of being
  imported as a blank. The status is set to "In progress" first and only
  marked complete at the end, so an interrupted run is picked up again. A
  dry run saves what would be written instead.
* `kv_cleanup()` deletes the local copies (step 7).
* `kv_run()` runs the whole workflow, processing several recordings at once
  with `workers`, and saves a log of each run.

## Processing changes

These change the processed values compared with the old `process_file()`:

* GGIR is pinned to 3.3-9 and GGIRread to 1.0.11. The previous
  `GGIR (>= 3.1-4)` floor allowed GGIR 3.1-4, whose non-wear detection missed
  non-wear in GENEActiv files.
* The timezone is set explicitly to Australia/Sydney. Previously GGIR used the
  system timezone, which is UTC in the devcontainer, so days were split at
  about 10 or 11 am local time.
* The light-intensity threshold is now 56.3 mg (Hildebrand et al. 2017,
  GENEActiv non-dominant wrist, children aged 7-11), matching the REDCap
  field notes. It was 51.6 mg (Hurter et al. 2018).
* Part 5 now uses the same 16-hour valid-day rule as part 2.
* Part 6 (circadian rhythm) is now run.
* Multiple files for one timepoint are appended by GGIR
  (`maxRecordingInterval`).
* Deprecated or no-op arguments were removed, including `strategy`, which is
  now `data_masking_strategy`.
