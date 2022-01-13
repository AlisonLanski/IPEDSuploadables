#' Shortcut function to do all steps to produce a 12 month Enrollment report
#'
#' @param df A dataframe set up according to the readme for students
#' @param hrs A dataframe set up according to the readme for instructional activity
#' @param part A string with what part of the reprt you want to produce: 'all', 'A', etc.
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @return A txt file at the path of your choice
#' @export
#'

produce_e1d_report <- function(df, hrs, part = "ALL", output = "full", format = "uploadable") {

  if (toupper(part) == "ALL") {
    # out of order because part A was expanded
    # and the expansion was called "C" in the upload
    make_e1d_part_A(df, output = output, format = format)
    make_e1d_part_C(df, output = output, format = format)
    make_e1d_part_B(hrs, output = output, format = format)
  }

  if (toupper(part) %in% c("A", "C")) {
    do.call(paste0("make_e1d_part_", toupper(part)), list(df, output = 'part', format))
  }

  if (toupper(part) == "B") {
    do.call(paste0("make_e1d_part_", toupper(part)), list(hrs, output = 'part', format))
  }
}

