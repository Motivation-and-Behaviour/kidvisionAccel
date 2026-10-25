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

# File uploads, by what they hold. A recording counts as processed once all of
# these are present (for records processed before the status fields existed).
file_fields <- c(
  part2 = "acc_pt2", part4 = "acc_pt4", part5 = "acc_pt5", part6 = "acc_pt6",
  report = "reports_accel"
)

# Processing status and provenance. These are not in the original project and
# are added to the accelerometer_output instrument by hand.
status_fields <- c(
  "cac_proc_status", "cac_proc_datetime", "cac_proc_ggir", "cac_proc_pkg",
  "cac_proc_params", "cac_proc_nfiles", "cac_proc_notes"
)

# Exported by kv_report() to personalise the report.
report_fields <- c("cid_namef", "pid_namef", "cid_dob", "consent_camera")

# Read to decide what to process and to personalise the report.
read_fields <- c(
  report_fields, "consent_geneactiv", "withdrawn_existingdata",
  "dropout_existingdata", "sop_date", "sop2_date", "sop2_postchecklist",
  "cac_gen_num"
)

# Exported by kv_pending() to decide what to process.
pending_fields <- c(
  "record_id", "consent_geneactiv", "withdrawn_existingdata",
  "dropout_existingdata", "sop_date", "sop2_date", "sop2_postchecklist",
  "cac_gen_num", unname(file_fields), "cac_proc_status", "cac_proc_ggir",
  "cac_proc_params"
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

# The REDCap values for a recording: a one-row data frame with the record ID,
# event and every derived field, as text. Fields without a value are NA, so an
# import with overwriting clears any value left from earlier processing.
to_redcap_record <- function(days, results, record_id, event) {
  per_day <- lapply(seq_len(n_measurement_days), function(n) {
    # Indexing past the last day gives a row of NAs
    values <- day_values(days[if (n <= nrow(days)) n else NA_integer_, ])
    stats::setNames(values, gsub("{n}", n, names(values), fixed = TRUE))
  })
  values <- c(
    record_id = record_id,
    redcap_event_name = event,
    unlist(per_day),
    summary_values(days, results)
  )
  tibble::as_tibble(as.list(values))
}

day_values <- function(day) {
  activity <- function(x) format_number(if (isTRUE(day$valid_day)) x else NA, 1)
  sleep <- function(x, digits) {
    format_number(if (isTRUE(day$valid_night)) x else NA, digits)
  }
  sleep_text <- function(x) if (isTRUE(day$valid_night)) x else NA_character_
  c(
    "cac_day{n}_calday" = format(day$date),
    "cac_day{n}_dotw" = as.character(day$weekday),
    "cac_day{n}_nhours" = format_number(day$hours, 2),
    "cac_day{n}_vhours" = format_number(day$valid_hours, 2),
    "cac_day{n}_full_sed" = activity(day$sedentary),
    "cac_day{n}_full_lpa" = activity(day$light),
    "cac_day{n}_full_mpa" = activity(day$moderate),
    "cac_day{n}_full_vpa" = activity(day$vigorous),
    "cac_day{n}_full_mvpa" = activity(day$mvpa),
    "cac_day{n}_noc" = sleep(day$sleep_hours, 2),
    "cac_day{n}_onset" = sleep(day$onset, 2),
    "cac_day{n}_onsetts" = sleep_text(day$onset_time),
    "cac_day{n}_wake" = sleep(day$wake, 2),
    "cac_day{n}_wakets" = sleep_text(day$wake_time),
    "cac_day{n}_timeinbed" = sleep(day$sleep_period_hours, 2),
    "cac_day{n}_eff" = sleep(day$sleep_efficiency, 3)
  )
}

summary_values <- function(days, results) {
  valid <- days[days$valid_day, ]
  nights <- days[days$valid_night, ]
  average <- function(x, digits) {
    format_number(if (any(!is.na(x))) mean(x, na.rm = TRUE) else NA, digits)
  }

  activity <- c(
    sed = "sedentary", lpa = "light", mpa = "moderate", vpa = "vigorous",
    mvpa = "mvpa"
  )
  out <- character()
  for (field in names(activity)) {
    minutes <- valid[[activity[[field]]]]
    out[paste0("cac_sum_", field)] <- average(minutes, 1)
    out[paste0("cac_sum_", field, "wd")] <- average(minutes[!valid$weekend], 1)
    out[paste0("cac_sum_", field, "we")] <- average(minutes[valid$weekend], 1)
  }

  person <- results$part2_person
  out[["cac_sum_enmo"]] <- format_number(
    as_number(person$`AD_mean_ENMO_mg_0-24hr`), 2
  )
  out[["cac_sum_noc"]] <- average(nights$sleep_hours, 2)
  out[["cac_sum_onset"]] <- average(nights$onset, 2)
  out[["cac_sum_wake"]] <- average(nights$wake, 2)
  out[["cac_sum_timeinbed"]] <- average(nights$sleep_period_hours, 2)

  out[["cac_gen_protodays"]] <- format_number(
    as_number(person$meas_dur_def_proto_day), 3
  )
  out[["cac_gen_weardays"]] <- format_number(
    as_number(person$wear_dur_def_proto_day), 3
  )
  out[["cac_gen_validwe"]] <- as.character(sum(valid$weekend))
  out[["cac_gen_validwd"]] <- as.character(sum(!valid$weekend))
  out[["cac_gen_calibstatus"]] <- person$calib_status
  out[["cac_gen_caliberror"]] <- format_number(as_number(person$calib_err), 4)
  out[["cac_gen_nights"]] <- results$part4_person$n_nights_acc
  out[["cac_gen_sleepdef"]] <- results$part4_night$sleepparam[1]
  out
}

# Numbers as plain decimals rounded to `digits` places (never scientific
# notation, which REDCap rejects), or NA.
format_number <- function(x, digits) {
  out <- formatC(
    round(x, digits), format = "f", digits = digits, drop0trailing = TRUE
  )
  out[is.na(x)] <- NA_character_
  out
}
