#' Shortcut function with all steps to provide a 12 Month Enrollment report
#'
#' @param df A dataframe set up according to the readme for students
#' @param hrs A dataframe set up according to the readme for instructional activity
#' @param part A string with what part of the report you want to produce: 'all', 'A', etc.
#' @param format A string (\code{"uploadable"} will produce a properly formatted
#'   upload file. \code{"readable"} will produce a csv of the upload file (only
#'   works for one part at a time). \code{"both"} will provide both options, but
#'   only works with one part at a time.
#'
#' @return A txt or csv file at the path of your choice
#' @export
#'

produce_e1d_report <- function(df, hrs, part = "ALL", format = "uploadable") {

  stopifnot(toupper(part) %in% c("A", "B", "C", "ALL"),
            toupper(format) %in% c("UPLOADABLE", "READABLE", "BOTH"))

  survey <- "12MonthEnrollment"
  output_path <- set_report_path()

  if (toupper(part) == "ALL" & toupper(format) == 'UPLOADABLE') {
    # out of order because part A was expanded
    # and the expansion was called "C" in the upload
    write_report(
      make_e1d_part_A(df),
      make_e1d_part_C(df),
      make_e1d_part_B(hrs),
      survey = survey,
      part = 'AllParts',
      output_path = output_path
    )
  } else if(toupper(part) %in% c("A", "B", "C")) {

    if(toupper(format) %in% c("UPLOADABLE", "BOTH")){

      if(toupper(part) == 'B'){
        write_report(
          make_e1d_part_B(hrs),
          survey = survey,
          part = paste0("Part", toupper(part)),
          output_path = output_path
        )
      } else {
        write_report(
          do.call(paste0("make_e1d_part_", toupper(part)), list(df)),
          survey = survey,
          part = paste0("Part", toupper(part)),
          output_path = output_path
          )
      }

    }

    if(toupper(format) %in% c("BOTH", "READABLE")){

      if(toupper(part) == 'B'){
        write_report_csv(
          make_e1d_part_B(hrs),
          survey = survey,
          part = paste0("Part", toupper(part)),
          output_path = output_path
          )
      } else {
        write_report_csv(
          do.call(paste0("make_e1d_part_", toupper(part)), list(df)),
          survey = survey,
          part = paste0("Part", toupper(part)),
          output_path = output_path
          )
      }
    }

  }

}

