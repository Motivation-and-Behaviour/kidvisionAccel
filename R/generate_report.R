#' Generate a report for the accelerometer data
#'
#' @param file_path The path to the accelerometer file.
#' Needs to be the same path as the `process_file` function.
#' @param participant_id The participant ID
#' @param parent_name Name of the parent
#' @param child_name Name of the child
#'
#' @return Path to the generated report
#' @export
generate_report <- function(
    file_path, participant_id, parent_name, child_name) {
  # Generate a temp dir
  temp_dir <- tempdir()
  html_file <- file.path(
    temp_dir,
    glue::glue("Participant Report {participant_id}.html")
  )
  pdf_file <- file.path(
    temp_dir,
    glue::glue("Participant Report {participant_id}.pdf")
  )
  # Generate the report
  rmarkdown::render(
    system.file("report/report_template.Rmd", package = "kidvisionAccel"),
    params = list(
      folder_path = file_path,
      participant_id = participant_id,
      parent_name = parent_name,
      child_name = child_name
    ),
    output_file = html_file
  )

  pagedown::chrome_print(
    input = html_file,
    output = pdf_file
  )

  stopifnot(file.exists(pdf_file))
  return(pdf_file)
}
