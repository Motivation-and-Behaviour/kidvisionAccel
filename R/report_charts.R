# Charts for the participant report, drawn with ggplot2 in the report's font
# and colours and saved as PNG files for the Typst template.

report_colours <- c(
  purple = "#4C1F8D",
  violet = "#8A5CD0",
  lavender = "#D7C9F1",
  ink = "#2B2340",
  grey = "#6E6880"
)
# Registered under its own name so a system copy of Lato can't be used instead.
report_font <- "KidVision Lato"

# Writes the charts the report needs to `dir`, skipping those without data.
write_report_charts <- function(data, dir) {
  register_report_font()
  charts <- list()
  if (data$activity$available) {
    charts$activity <- chart_activity(data$activity$table)
    charts$day <- chart_day(data$day)
  }
  if (data$sleep$available) {
    charts$sleep <- chart_sleep(data$sleep$table, data$sleep$guideline_range)
    charts$timing <- chart_timing(data$sleep$table)
    charts$efficiency <- chart_efficiency(data$sleep$table)
  }
  heights <- c(
    activity = 3, day = 1.5, sleep = 2.7, timing = 2.8, efficiency = 2.6
  )
  for (name in names(charts)) {
    save_chart(
      charts[[name]], file.path(dir, paste0(name, ".png")), heights[[name]]
    )
  }
  invisible(names(charts))
}

save_chart <- function(chart, path, height) {
  ragg::agg_png(
    path,
    width = 6.3, height = height, units = "in", res = 300,
    background = "white"
  )
  # Closed even if drawing fails, so failed reports don't leave devices open
  on.exit(grDevices::dev.off())
  print(chart)
}

register_report_font <- function() {
  if (report_font %in% systemfonts::registry_fonts()$family) {
    return(invisible())
  }
  font <- function(style) {
    system.file(
      "report", "fonts", paste0("Lato-", style, ".ttf"),
      package = "kidvisionAccel"
    )
  }
  systemfonts::register_font(
    report_font,
    plain = font("Regular"), bold = font("Bold"), italic = font("Italic"),
    bolditalic = font("BoldItalic")
  )
}

report_theme <- function() {
  ggplot2::theme_minimal(base_family = report_font, base_size = 12) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = report_colours[["ink"]]),
      axis.text = ggplot2::element_text(colour = report_colours[["grey"]]),
      axis.title = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(4, 4, 4, 4)
    )
}

# Days in date order, labelled with the weekday above the date, so repeated
# weekdays in a longer recording stay separate.
date_axis <- function(table) {
  labels <- paste0(
    format(table$date, "%a"), "\n", trimws(format(table$date, "%e %b"))
  )
  factor(labels, levels = labels[order(table$date)])
}

# Charts with many bars get smaller, shorter value labels so they don't
# overlap.
crowded <- function(table) nrow(table) > 9

value_size <- function(table) if (crowded(table)) 2.7 else 3.4

# Value labels on the bars or ranges of a chart.
value_text <- function(table, ..., shrink = 0) {
  ggplot2::geom_text(
    ..., family = report_font, size = value_size(table) - shrink,
    colour = report_colours[["ink"]]
  )
}

crowded_axis <- function(table) {
  if (crowded(table)) {
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8.5))
  }
}

chart_activity <- function(table) {
  table$day <- date_axis(table)
  top <- max(75, max(table$mvpa) * 1.15)
  ggplot2::ggplot(table, ggplot2::aes(.data$day, .data$mvpa)) +
    ggplot2::geom_col(fill = report_colours[["purple"]], width = 0.7) +
    ggplot2::geom_hline(
      yintercept = 60, linetype = "dashed", colour = report_colours[["ink"]]
    ) +
    value_text(
      table, ggplot2::aes(label = paste(round(.data$mvpa))), vjust = -0.5
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, top), expand = c(0, 0),
      labels = function(x) paste(x, "min")
    ) +
    report_theme() +
    crowded_axis(table)
}

# Each night's sleep period, split into the time scored as asleep and the
# time awake or restless, against the recommended range.
chart_sleep <- function(table, guideline) {
  table$night <- date_axis(table)
  parts <- rbind(
    data.frame(night = table$night, part = "Asleep", hours = table$asleep),
    data.frame(
      night = table$night, part = "Awake or restless",
      hours = table$period - table$asleep
    )
  )
  parts$part <- factor(parts$part, levels = c("Awake or restless", "Asleep"))
  table$label <- if (crowded(table)) {
    minutes <- round(table$period * 60)
    sprintf("%d:%02d", minutes %/% 60, minutes %% 60)
  } else {
    vapply(table$period, format_hours, "")
  }
  top <- max(guideline[2] + 1.5, max(table$period) + 1)
  ggplot2::ggplot(parts, ggplot2::aes(.data$night, .data$hours)) +
    ggplot2::annotate(
      "rect", xmin = -Inf, xmax = Inf, ymin = guideline[1],
      ymax = guideline[2], fill = "#EFE8FA"
    ) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$part), width = 0.7) +
    value_text(
      table, ggplot2::aes(.data$night, .data$period, label = .data$label),
      data = table, vjust = -0.5
    ) +
    ggplot2::scale_fill_manual(values = c(
      "Asleep" = report_colours[["purple"]],
      "Awake or restless" = report_colours[["violet"]]
    ), breaks = c("Asleep", "Awake or restless")) +
    ggplot2::scale_y_continuous(
      limits = c(0, top), breaks = seq(0, 14, by = 2), expand = c(0, 0),
      labels = function(x) paste(x, "h")
    ) +
    report_theme() +
    crowded_axis(table)
}

# Bedtime to wake-up for each night, on a clock running from 6 pm to noon.
chart_timing <- function(table) {
  table$night <- date_axis(table)
  table$type <- ifelse(table$weekend, "Friday and Saturday nights",
    "Other nights"
  )
  breaks <- seq(18, 36, by = 2)
  # Without am and pm when crowded: bedtimes are evening, wake-ups morning
  clock <- if (crowded(table)) {
    function(hours) sub(" [ap]m$", "", format_clock(hours))
  } else {
    short_clock
  }
  ggplot2::ggplot(table, ggplot2::aes(x = .data$night)) +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$onset, ymax = .data$wake, colour = .data$type),
      linewidth = 9
    ) +
    # The axis is reversed, so subtracting moves a label up the page
    value_text(
      table,
      ggplot2::aes(
        y = .data$onset - 0.6, label = vapply(.data$onset, clock, "")
      ),
      shrink = 0.4
    ) +
    value_text(
      table,
      ggplot2::aes(y = .data$wake + 0.6, label = vapply(.data$wake, clock, "")),
      shrink = 0.4
    ) +
    ggplot2::scale_y_reverse(
      breaks = breaks, labels = vapply(breaks, axis_clock, ""),
      limits = c(max(36, max(table$wake) + 1), min(18, min(table$onset) - 1))
    ) +
    ggplot2::scale_colour_manual(values = c(
      "Other nights" = report_colours[["purple"]],
      "Friday and Saturday nights" = report_colours[["violet"]]
    )) +
    report_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(
      colour = "grey90"
    )) +
    crowded_axis(table)
}

chart_efficiency <- function(table) {
  table$night <- date_axis(table)
  ggplot2::ggplot(table, ggplot2::aes(.data$night, .data$efficiency)) +
    ggplot2::geom_col(fill = report_colours[["violet"]], width = 0.7) +
    value_text(
      table,
      ggplot2::aes(label = vapply(.data$efficiency, format_percent, "")),
      vjust = -0.5
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1.12), breaks = seq(0, 1, by = 0.25), expand = c(0, 0),
      labels = function(x) paste0(x * 100, "%")
    ) +
    report_theme() +
    crowded_axis(table)
}

# An average day as one bar split into its parts.
chart_day <- function(day) {
  labels <- c(
    "Asleep", "Awake in the night", "Sitting or still", "Light activity",
    "Moderate-to-vigorous activity"
  )
  parts <- tibble::tibble(
    part = factor(labels, levels = labels),
    hours = unlist(day[c("sleep", "awake_night", "sitting", "light", "mvpa")])
  )
  ggplot2::ggplot(parts, ggplot2::aes(1, .data$hours, fill = .data$part)) +
    ggplot2::geom_col(
      width = 0.6, position = ggplot2::position_stack(reverse = TRUE)
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 24, by = 3), labels = function(x) paste(x, "h"),
      expand = ggplot2::expansion(add = c(0.4, 0.4))
    ) +
    ggplot2::scale_fill_manual(values = c(
      "Asleep" = report_colours[["purple"]],
      "Awake in the night" = "#6B3FB0",
      "Sitting or still" = report_colours[["lavender"]],
      "Light activity" = "#B79BE6",
      "Moderate-to-vigorous activity" = "#F2A541"
    )) +
    report_theme() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank()
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2))
}

# A clock time without minutes when on the hour, e.g. "9 pm" or "9:30 pm".
short_clock <- function(hours) {
  sub(":00", "", format_clock(hours), fixed = TRUE)
}

# Clock times for an axis, with "midnight" and "noon" rather than 12 am and
# 12 pm, which are easily confused.
axis_clock <- function(hours) {
  if (hours %% 24 == 0) "midnight" else if (hours %% 24 == 12) "noon" else
    short_clock(hours)
}
