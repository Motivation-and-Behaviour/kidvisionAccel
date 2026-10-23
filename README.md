# kidvisionAccel

Processes accelerometer data for the KidVision project. It finds the
participants whose data have not yet been processed in REDCap, copies their
raw GENEActiv files from OneDrive, runs a pinned version of
[GGIR](https://github.com/wadpac/GGIR), writes the results back to REDCap, and
creates a report for each family.

`vignette("kidvisionAccel")` explains the pipeline in detail, including the
GGIR settings and how days and nights are defined.


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
work folder has a row for each recording. Each recording's processing status
and notes are also recorded in REDCap.

`vignette("kidvisionAccel")` covers the rest: running the steps one at a
time, how the OneDrive folders should be organised (and
`kv_audit_folders()`, which lists folders to fix), what each processing
status means and what to do about it, and how to change the GGIR version or
settings.

Copying a file from OneDrive downloads it, and OneDrive then keeps it on the
computer. To free that space again automatically, turn on Storage Sense in
Windows and set OneDrive content to become online-only when it hasn't been
opened for a while.

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
