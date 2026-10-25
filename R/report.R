#' Create the participant reports
#'
#' Step 5 of the workflow. Creates a PDF report for each recording merged by
#' [kv_collate()], for the child and their family. It summarises the child's
#' physical activity and sleep against the Australian 24-Hour Movement
#' Guidelines, using the same day table as the values sent to REDCap.
#'
#' The child's and parent's first names and the child's date of birth (for the
#' age-appropriate sleep guideline) are read from REDCap. Names appear only in
#' the report itself, which is named `KidVision_<record_id>_<timepoint>.pdf`.
#'
#' @param jobs A job table from [kv_collate()].
#' @param config A [kv_config()] object.
#'
#' @return The job table, with the path of each report in `report`.
#'   Recordings with a report have status `"reported"`; recordings whose
#'   report couldn't be created have status `"report_failed"`.
#' @export
kv_report <- function(jobs, config = kv_config()) {
  if (!"report" %in% names(jobs)) jobs$report <- NA_character_
  rows <- which(jobs$status == "collated")
  if (length(rows) == 0) {
    return(report_jobs(jobs, "Report", rows))
  }

  people <- participant_details(config, unique(jobs$record_id[rows]))
  for (i in rows) {
    person <- people[people$record_id == jobs$record_id[[i]], ]
    path <- file.path(
      jobs$job_dir[[i]], "out",
      paste0("KidVision_", job_name(jobs, i), ".pdf")
    )
    jobs <- tryCatch(
      {
        data <- report_data(
          jobs$days[[i]],
          child = person$cid_namef[1],
          parent = person$pid_namef[1],
          dob = as.Date(person$cid_dob[1]),
          timepoint = jobs$timepoint[[i]],
          camera = person$consent_camera[1] %in% "1"
        )
        render_report(data, path)
        jobs$report[[i]] <- path
        set_status(jobs, i, "reported")
      },
      error = function(e) {
        set_status(jobs, i, "report_failed", paste(
          "The report couldn't be created:", conditionMessage(e)
        ))
      }
    )
  }

  report_jobs(jobs, "Report", rows)
}

participant_details <- function(config, record_ids) {
  rcon <- rc_connect(config)
  details <- rc_export(
    rcon,
    fields = c("record_id", report_fields),
    events = enrolment_event,
    records = record_ids
  )
  for (field in report_fields) {
    details[[field]] <- blank_to_na(details[[field]], nrow(details))
  }
  details
}

# Everything the report shows, from a recording's day table. Text is
# formatted here so the template only lays it out.
report_data <- function(days, child = NA, parent = NA, dob = NA,
                        timepoint = "baseline", camera = FALSE) {
  valid <- days[days$valid_day, ]
  nights <- days[days$valid_night, ]
  age <- age_on(dob, days$date[1])
  guideline <- sleep_guideline(age)

  activity <- tibble::tibble(date = valid$date, mvpa = valid$mvpa)
  sleep <- tibble::tibble(
    date = nights$date,
    # Friday and Saturday nights come before a weekend day
    weekend = nights$weekday %in% c(5, 6),
    # The sleep period, from falling asleep to waking, which is compared with
    # the guideline, and the part of it the monitor scored as asleep
    period = nights$sleep_period_hours,
    asleep = nights$sleep_hours,
    efficiency = nights$sleep_efficiency,
    onset = nights$onset,
    wake = nights$wake
  )

  list(
    child_label = if (is.na(child)) "Your child" else child,
    child_possessive = possessive(child),
    parent = if (is.na(parent)) NA_character_ else parent,
    # Whether the child also wore the study's camera, for the thank-you letter
    camera = camera,
    timepoint_label = timepoint_label(timepoint),
    first_day = format_date(days$date[1]),
    last_day = format_date(days$date[nrow(days)]),
    age = age,
    activity = list(
      available = nrow(activity) > 0,
      days = nrow(activity),
      days_meeting = sum(activity$mvpa >= 60),
      average = format_minutes(mean(activity$mvpa)),
      table = activity
    ),
    sleep = list(
      available = nrow(sleep) > 0,
      nights = nrow(sleep),
      average = format_hours(mean(sleep$period)),
      guideline = paste0(guideline[1], " to ", guideline[2], " hours"),
      guideline_range = guideline,
      bedtime = format_clock(stats::median(sleep$onset)),
      wake = format_clock(stats::median(sleep$wake)),
      efficiency = format_percent(mean(sleep$efficiency)),
      table = sleep
    ),
    day = average_day(valid)
  )
}

# How an average valid day was spent, in hours. Each midnight-to-midnight
# window is scaled to 24 hours first, as daylight saving changes make some
# 23 or 25 hours long.
average_day <- function(valid) {
  parts <- c(
    sleep = "sleep_period_sleep", awake_night = "sleep_period_awake",
    sitting = "sedentary", light = "light", mvpa = "mvpa"
  )
  if (nrow(valid) == 0) {
    return(stats::setNames(as.list(rep(NA_real_, length(parts))), names(parts)))
  }
  hours <- vapply(parts, function(column) {
    mean(valid[[column]] / valid$window * 24, na.rm = TRUE)
  }, numeric(1))
  as.list(hours)
}

# Sleep recommended by the Australian 24-Hour Movement Guidelines, in hours.
sleep_guideline <- function(age) {
  if (is.na(age) || (age >= 5 && age <= 13)) {
    c(9, 11)
  } else if (age < 5) {
    c(10, 13)
  } else {
    c(8, 10)
  }
}

age_on <- function(dob, date) {
  if (is.na(dob) || is.na(date)) {
    return(NA_integer_)
  }
  years <- as.integer(format(date, "%Y")) - as.integer(format(dob, "%Y"))
  birthday_to_come <- format(date, "%m%d") < format(dob, "%m%d")
  years - birthday_to_come
}

possessive <- function(name) {
  if (is.na(name)) {
    return("Your child\u2019s")
  }
  # A typographic apostrophe, to match the template's text
  paste0(name, if (grepl("s$", name)) "\u2019" else "\u2019s")
}

timepoint_label <- function(timepoint) {
  c(
    baseline = "First visit", `12m` = "12-month visit",
    `24m` = "24-month visit"
  )[[timepoint]]
}

# Formatting for the report ----------------------------------------------------

format_date <- function(date) {
  trimws(format(date, "%e %B %Y"))
}

format_minutes <- function(minutes) {
  if (is.na(minutes)) {
    return(NA_character_)
  }
  minutes <- round(minutes)
  if (minutes < 60) {
    return(paste(minutes, "min"))
  }
  if (minutes %% 60 == 0) {
    return(paste(minutes %/% 60, "h"))
  }
  paste0(minutes %/% 60, " h ", minutes %% 60, " min")
}

format_hours <- function(hours) format_minutes(hours * 60)

# Hours since midnight (which can be over 24 for times after the next
# midnight) as a clock time such as "8:45 pm".
format_clock <- function(hours) {
  if (is.na(hours)) {
    return(NA_character_)
  }
  minutes <- round((hours %% 24) * 60) %% (24 * 60)
  hour <- minutes %/% 60
  suffix <- if (hour < 12) "am" else "pm"
  hour12 <- if (hour %% 12 == 0) 12 else hour %% 12
  sprintf("%d:%02d %s", hour12, minutes %% 60, suffix)
}

format_percent <- function(fraction) {
  if (is.na(fraction)) NA_character_ else paste0(round(fraction * 100), "%")
}

# Renders the report to a PDF at `output`: the template, charts and data go
# into a temporary folder, and Quarto's bundled Typst compiles them.
render_report <- function(data, output) {
  dir <- tempfile("report")
  fs::dir_copy(report_template_dir(), dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  write_report_charts(data, dir)
  jsonlite::write_json(
    report_json(data), file.path(dir, "data.json"),
    auto_unbox = TRUE, na = "null", null = "null", digits = NA
  )

  pdf <- file.path(dir, "report.pdf")
  result <- suppressWarnings(system2(
    quarto_path(),
    c(
      "typst", "compile", shQuote(file.path(dir, "report.typ")),
      shQuote(pdf), "--font-path", shQuote(file.path(dir, "fonts")),
      "--ignore-system-fonts"
    ),
    stdout = TRUE, stderr = TRUE
  ))
  if (!is.null(attr(result, "status")) || !file.exists(pdf)) {
    stop("Typst failed: ", paste(result, collapse = "\n"), call. = FALSE)
  }
  fs::dir_create(dirname(output))
  fs::file_copy(pdf, output, overwrite = TRUE)
  invisible(output)
}

report_template_dir <- function() {
  system.file("report", package = "kidvisionAccel", mustWork = TRUE)
}

quarto_path <- function() {
  path <- Sys.which("quarto")
  if (!nzchar(path)) {
    stop("Quarto wasn't found; it is needed to create reports.", call. = FALSE)
  }
  path
}

# The data the Typst template reads. Tables become arrays of rows, and dates
# and numbers the template shows are already formatted as text.
report_json <- function(data) {
  data$activity$table <- NULL
  data$sleep$table <- NULL
  data$sleep$guideline_range <- NULL
  data$day <- lapply(data$day, format_hours)
  data
}
