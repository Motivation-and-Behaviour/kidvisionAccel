# kidvisionAccel

Processes accelerometer data for the KidVision project. It finds the
participants whose data have not yet been processed in REDCap, copies their
raw GENEActiv files from OneDrive, runs a pinned version of
[GGIR](https://github.com/wadpac/GGIR), writes the results back to REDCap, and
creates a report for each family.


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

Process everything that hasn't been processed yet:

``` r
library(kidvisionAccel)
kv_run()
```

`kv_run()` checks that everything is in place, finds the recordings to
process in REDCap, copies their files from OneDrive, runs GGIR, writes the
results, files and participant report to REDCap, and deletes the local
copies. GGIR takes about 3 minutes per recording; set `KIDVISION_WORKERS` (or
`workers =`) to process several at once, allowing about 3 GB of memory each.

Some useful variations:

``` r
# Try some recordings without writing anything to REDCap. What would have
# been written is saved in dry_run/ in the work folder.
kv_run(ids = c("1009", "4340"), timepoints = "baseline", dry_run = TRUE)

# Retry recordings that needed review, once they are fixed on OneDrive
kv_run(ids = "1020")

# Reprocess everything done with a different GGIR version or settings
kv_run(reprocess = "outdated")
```

A summary is printed at the end, and `logs/run_<date and time>.csv` in the
work folder has a row for each recording.

The steps can also be run one at a time. Each takes and returns the job
table, with one row per recording: `status` shows how far each recording
got, and `message` says why it stopped.

``` r
kv_check()
jobs <- kv_pending()      # recordings that need processing, from REDCap
jobs <- kv_locate(jobs)   # find their files on OneDrive (nothing downloaded)
jobs <- kv_fetch(jobs)    # copy and check the files
jobs <- kv_process(jobs)  # run GGIR
jobs <- kv_collate(jobs)  # merge GGIR's results and prepare REDCap values
jobs <- kv_report(jobs)   # create the participant reports
jobs <- kv_write(jobs)    # write everything to REDCap
kv_cleanup(jobs)          # delete the local copies
```

Each recording's local files are in `jobs/<record_id>_<timepoint>/` in the
work folder until they are cleaned up: the copied raw files in `raw/`, GGIR's
output in `ggir/`, and the files for REDCap, including the report, in `out/`.
GGIR's messages are kept in `logs/`.

The report's wording and layout are in `inst/report/report.typ`, a
[Typst](https://typst.app/docs/) template; the charts are drawn in
`R/report_charts.R`.

Copying a file from OneDrive downloads it, and OneDrive then keeps it on the
computer. To free that space again automatically, turn on Storage Sense in
Windows and set OneDrive content to become online-only when it hasn't been
opened for a while.

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

## Processing status in REDCap

The pipeline records what happened to each recording in `cac_proc_status`,
with the details in `cac_proc_notes`:

| Status | Meaning | What happens next |
| --- | --- | --- |
| 0, In progress | A run stopped part-way | Processed again on the next run |
| 1, Complete | Results, files and report are in REDCap | Only reprocessed if asked (`reprocess`) |
| 2, Complete - check notes | As above, with something worth checking, such as a device swap or more than 10 days | As above |
| 3, No file found | No folder or `.bin` file on OneDrive | Processed once a `.bin` file appears |
| 4, No valid days | No day or night had enough wear | Only when named in `ids` |
| 5, Failed (will retry) | Copying, GGIR (out of time) or the report failed | Processed again on the next run |
| 6, Needs manual review | A folder needs renaming, or the files need a decision | Fix on OneDrive, then run with `ids` |
| 7, Failed (cannot be processed) | GGIR couldn't process the file, or it is corrupt or incomplete | Only when named in `ids` |

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
