# kidvisionAccel 0.0.0.9000

The package is being rebuilt into a full processing pipeline. This entry
covers the foundation stage.

## Breaking changes

* `process_file()`, `generate_report()` and `upload_report()` have been
  removed. Later stages will replace them with step functions and a single
  `kv_run()` entry point. `upload_report()` also used REDCap event names that
  do not exist for the 12- and 24-month visits.

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
  before visit 1 (step 2).
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

## Processing changes

These change the processed values compared with the old `process_file()`:

* GGIR is pinned to 3.3-9 and GGIRread to 1.0.11. The previous
  `GGIR (>= 3.1-4)` floor allowed GGIR 3.1-4, whose non-wear detection missed
  non-wear in GENEActiv files.
* The timezone is set explicitly to Australia/Sydney. Previously GGIR used the
  system timezone, which is UTC in the devcontainer, so days were split at
  about 10 or 11 am local time.
* The light-intensity threshold is now 56.3 mg (Hildebrand et al. 2017, GENEActiv
  non-dominant wrist, children aged 7-11), matching the REDCap field notes. It
  was 51.6 mg (Hurter et al. 2018).
* Part 5 now uses the same 16-hour valid-day rule as part 2.
* Part 6 (circadian rhythm) is now run.
* Multiple files for one timepoint are appended by GGIR
  (`maxRecordingInterval`).
* Deprecated or no-op arguments were removed, including `strategy`, which is
  now `data_masking_strategy`.
