#' Shortcut function to do all steps to produce a 12 month Enrollment report
#'
#' @param df A dataframe set up according to the readme for students
#' @param hrs A dataframe set up according to the readme for instructional activity
#' @param part A string with what part of the reprt you want to produce: 'all', 'A', etc.
#'
#' @return A txt file at the path of your choice
#' @export
#'
produce_e1d_report <- function(df, hrs, part = "ALL") {


  if (toupper(part) == "ALL") {
    # out of order because part A was expanded
    # and the expansion was called "C" in the upload
    make_e1d_part_A(df, output = "full")
    make_e1d_part_C(df, output = "full")
    make_e1d_part_B(hrs, output = "full")

  }

  if (toupper(part) %in% c("A", "C")) {
    do.call(paste0("make_e1d_part_", toupper(part)), list(df))
  }

  if (toupper(part) == "B") {
    do.call(paste0("make_e1d_part_", toupper(part)), list(hrs))
  }
}

