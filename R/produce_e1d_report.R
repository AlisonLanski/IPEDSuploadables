#' Shortcut function with all steps to provide a 12 Month Enrollment report
#'
#' @param df A dataframe set up according to the readme for students
#' @param hrs A dataframe set up according to the readme for instructional activity
#' @param part A string with what part of the report you want to produce: 'all', 'A', etc.
#' @param format A string (\code{"uploadable"} will produce a properly formatted
#'   upload file. \code{"readable"} will produce a csv of the upload file (only
#'   works for one part at a time). \code{"both"} will provide both options, but
#'   only works with one part at a time.
#' @param ugender A boolean: TRUE means you are collecting and able to report
#'   "another gender" for undergraduate students Set as FALSE if necessary
#' @param ggender A boolean: TRUE means you are collecting and able to report
#'   "another gender" for graduate students. Set as FALSE if necessary
#'
#' @return A txt or csv file at the path of your choice
#' @export
#'
#'@examples
#'\dontshow{
#' #set temp directory for this example (not necessary for users)
#'.old_wd <- setwd(tempdir())
#'}
#'
#'#entire report
#'produce_e1d_report(e1d_student, e1d_instr)
#'
#'#one part, as csv instead of key-value file
#'produce_e1d_report(e1d_student, part = "A", format = "readable")
#'
#'\dontshow{
#' #reset directory for this example (not necessary for users)
#'setwd(.old_wd)
#'}

produce_e1d_report <- function(df, hrs, part = "ALL", format = "uploadable",
                               ugender = TRUE, ggender = TRUE) {

  stopifnot(toupper(part) %in% c("A", "B", "C", "D", "ALL"),
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
      make_e1d_part_D(df, ugender, ggender),
      survey = survey,
      part = 'AllParts',
      output_path = output_path
    )
  } else if(toupper(part) %in% c("A", "B", "C", "D")) {

    if(toupper(format) %in% c("UPLOADABLE", "BOTH")){

      if(toupper(part) == 'B'){
        write_report(
          make_e1d_part_B(hrs),
          survey = survey,
          part = paste0("Part", toupper(part)),
          output_path = output_path
        )
      } else if(toupper(part) == 'D'){
        write_report(
          make_e1d_part_D(df, ugender, ggender),
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
      } else if(toupper(part) == 'D'){
        write_report_csv(
          make_e1d_part_D(df, ugender, ggender),
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

