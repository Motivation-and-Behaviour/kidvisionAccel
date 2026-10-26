# Small helpers used across the workflow steps.

as_number <- function(x) suppressWarnings(as.numeric(x))

# The local calendar date of each time. as.Date() would give the UTC date
# before R 4.3.
local_date <- function(time) as.Date(format(time, "%Y-%m-%d"))

# A column of a REDCap export as trimmed text with blanks as NA. `x` is NULL
# when REDCap didn't return the field, which gives `n` NAs.
blank_to_na <- function(x, n = length(x)) {
  x <- if (is.null(x)) rep(NA_character_, n) else trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}
