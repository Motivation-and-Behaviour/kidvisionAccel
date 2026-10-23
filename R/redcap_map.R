# REDCap fields that the pipeline reads or writes. `kv_check()` confirms they
# all exist in the project before a run.

# Written for each measurement day; `{n}` is the day number (1 to 10).
day_field_stems <- c(
  # accelerometer_time
  "cac_day{n}_calday", "cac_day{n}_dotw", "cac_day{n}_nhours",
  "cac_day{n}_vhours",
  # accelerometer_physical_activity
  "cac_day{n}_full_sed", "cac_day{n}_full_lpa", "cac_day{n}_full_mpa",
  "cac_day{n}_full_vpa", "cac_day{n}_full_mvpa",
  # accelerometer_sleep
  "cac_day{n}_noc", "cac_day{n}_onset", "cac_day{n}_onsetts",
  "cac_day{n}_wake", "cac_day{n}_wakets", "cac_day{n}_timeinbed",
  "cac_day{n}_eff"
)

n_measurement_days <- 10

# Written once per recording. cac_gen_wrist and cac_gen_num are deliberately
# absent: research staff enter them, and the pipeline never overwrites them.
summary_fields <- c(
  # accelerometer_details
  "cac_gen_protodays", "cac_gen_weardays", "cac_gen_validwe",
  "cac_gen_validwd", "cac_gen_calibstatus", "cac_gen_caliberror",
  "cac_gen_nights", "cac_gen_sleepdef",
  # accelerometer_physical_activity
  "cac_sum_enmo",
  paste0(
    "cac_sum_",
    rep(c("sed", "lpa", "mpa", "vpa", "mvpa"), each = 3),
    c("", "wd", "we")
  ),
  # accelerometer_sleep
  "cac_sum_noc", "cac_sum_onset", "cac_sum_wake", "cac_sum_timeinbed"
)

# File uploads. A recording counts as processed once all of these are present
# (for records processed before the status fields existed).
file_fields <- c("acc_pt2", "acc_pt4", "acc_pt5", "acc_pt6", "reports_accel")

# Processing status and provenance. These are not in the original project and
# are added to the accelerometer_output instrument by hand.
status_fields <- c(
  "cac_proc_status", "cac_proc_datetime", "cac_proc_ggir", "cac_proc_pkg",
  "cac_proc_params", "cac_proc_nfiles", "cac_proc_notes"
)

# Read to decide what to process and to personalise the report.
read_fields <- c(
  "cid_namef", "pid_namef", "cid_dob", "consent_geneactiv",
  "withdrawn_existingdata", "dropout_existingdata",
  "sop_date", "sop2_date", "sop2_postchecklist", "cac_gen_num"
)

redcap_fields <- function() {
  day_fields <- unlist(lapply(
    seq_len(n_measurement_days),
    function(n) gsub("{n}", n, day_field_stems, fixed = TRUE)
  ))
  roles <- list(
    derived = c(day_fields, summary_fields),
    file = file_fields,
    status = status_fields,
    read = read_fields
  )
  tibble::tibble(
    field = unlist(roles, use.names = FALSE),
    role = rep(names(roles), lengths(roles))
  )
}
