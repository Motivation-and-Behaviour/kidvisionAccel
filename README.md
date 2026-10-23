# kidvisionAccel

Processes accelerometer data for the KidVision project. It finds the
participants whose data have not yet been processed in REDCap, copies their
raw GENEActiv files from OneDrive, runs a pinned version of
[GGIR](https://github.com/wadpac/GGIR), writes the results back to REDCap, and
creates a report for each family.

> **Status:** under active development. Everything up to creating the
> participant reports is available; writing to REDCap and the single
> `kv_run()` entry point are being added in stages.

## Setup

The package is meant to run in the devcontainer in this repository, under
WSL2.

1. In WSL, point `KIDVISION_ONEDRIVE` at the synced *Square Eyes - Community
   Sample* folder before opening the devcontainer, e.g. in `~/.profile`:

   ```sh
   export KIDVISION_ONEDRIVE="/mnt/c/Users/<you>/Australian Catholic University/Square Eyes - Community Sample"
   ```

   The folder is mounted read-only at `/onedrive`. Temporary files go to
   `~/kidvision_work` in WSL, mounted at `/work`.

2. Add your REDCap API token to `.env` in the repository root, which is
   ignored by git:

   ```sh
   REDCAP_KEY=<your token>
   ```

3. Open the folder in the devcontainer. The package's dependencies, including
   the pinned GGIR and GGIRread, are installed when the container is
   created.

Settings can be overridden with these environment variables:

| Variable | Default | Purpose |
| --- | --- | --- |
| `REDCAP_URI` | `https://redcap.acu.edu.au/api/` | REDCap API endpoint |
| `REDCAP_KEY` | none | REDCap API token |
| `KIDVISION_SOURCE_DIR` | `/onedrive` in the devcontainer | Synced folder with one folder per participant |
| `KIDVISION_WORK_DIR` | `/work` in the devcontainer | Local folder for copies, GGIR output and logs |
| `KIDVISION_TZ` | `Australia/Sydney` | Timezone the data were collected in |
| `KIDVISION_WORKERS` | `1` | Recordings to process in parallel |

## Usage

Check that everything is in place, then find the recordings to process and
copy their files:

``` r
library(kidvisionAccel)
kv_check()

jobs <- kv_pending()      # recordings that need processing, from REDCap
jobs <- kv_locate(jobs)   # find their files on OneDrive (nothing downloaded)
jobs <- kv_fetch(jobs)    # copy and check the files
jobs <- kv_process(jobs)  # run GGIR
jobs <- kv_collate(jobs)  # merge GGIR's results and prepare REDCap values
jobs <- kv_report(jobs)   # create the participant reports
```

GGIR takes about 3 minutes per recording. Its messages are saved in
`logs/` in the work folder, and each recording's files are in
`jobs/<record_id>_<timepoint>/`: the copied raw files in `raw/`, GGIR's
output in `ggir/`, and the files for REDCap, including the report, in
`out/`.

The report's wording and layout are in `inst/report/report.typ`, a
[Typst](https://typst.app/docs/) template; the charts are drawn in
`R/report_charts.R`.

Each step returns the job table, with one row per recording. `status` shows
how far each recording got, and `message` says why it stopped.

To list OneDrive folders that need renaming or fixing:

``` r
kv_audit_folders()
```

## OneDrive folders

Raw files are expected in
`<record_id>/<Baseline|Time_1|Time_2>/Accelerometer/`. Files in subfolders of
`Accelerometer` are used too, except in a subfolder called `Excluded`, which
is the place for files that shouldn't be processed. Folders with other names
(such as `Time 1` or `Accelerometers`) are reported rather than guessed at.

When a recording has more than one `.bin` file:

* copies of the same file are ignored;
* if each device started 24 hours or more after the one before, and no more
  than 3 days after it stopped, it's a device swap, and the files are joined
  into one recording;
* devices started less than 24 hours apart might have been worn at the same
  time, for example by a parent, and a device started more than 3 days after
  the previous one stopped looks like a second attempt, so these recordings
  are held for review. Move the files that shouldn't be used into
  `Excluded`.

Start and stop times come from inside the files, so these checks happen
after copying. The time in a GENEActiv file's name is when it was downloaded,
not when it started recording.

## REDCap processing fields

The pipeline records its progress in these fields on the
`accelerometer_output` instrument. They were added by hand: the pipeline
never changes the REDCap project's design itself.

| Field | Type | Label | Validation / choices |
| --- | --- | --- | --- |
| `cac_proc_status` | Dropdown | Processing status | `0, In progress \| 1, Complete \| 2, Complete - check notes \| 3, No file found \| 4, No valid days \| 5, Failed (will retry) \| 6, Needs manual review \| 7, Failed (cannot be processed)` |
| `cac_proc_datetime` | Text | Processed at (Sydney time) | Datetime w/ seconds (Y-M-D H:M:S) |
| `cac_proc_ggir` | Text | GGIR version | |
| `cac_proc_pkg` | Text | kidvisionAccel version | |
| `cac_proc_params` | Text | GGIR settings hash | |
| `cac_proc_nfiles` | Text | Number of .bin files used | Integer, 0-10 |
| `cac_proc_notes` | Notes | Processing notes | |

`kv_check()` fails a live run until these exist; a dry run only warns.
