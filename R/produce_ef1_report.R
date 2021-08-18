#' Shortcut function to do all steps to produce a report for Fall Enrollment
#'
#' @param students A dataframe set up according to the readme with student data
#' @param retention A dataframe set up according to the readme with retention data
#' @param cips A boolean indicating if this is a year with a CIP-breakout for Part A (TRUE or FALSE)
#' @param part A string with what part of the report you want to produce: 'all', 'A', etc.
#'
#' @return A txt file at the path of your choice
#' @export
#'
produce_ef1_report <- function(students, retention, cips = FALSE, part = "ALL") {

  students <- prep_ef1_data_frame(students)

  if (toupper(part) == "ALL") {
    make_ef1_part_A(df = students, cips = cips, output = "full")
    make_ef1_part_G(df = students, output = "full")
    make_ef1_part_B(df = students, output = "full")
    make_ef1_part_C(df = students, output = "full")
    make_ef1_part_D(df = students, output = "full")
    make_ef1_part_E(df = retention, output = "full")
    make_ef1_part_F(df = students, output = "full")
  }

  if(toupper(part) == "A"){
    do.call(paste0("make_ef1_part_", toupper(part)), list(students, cips))
  }
  if (toupper(part) %in% c("G", "B", "C", "D", "F")) {
    do.call(paste0("make_ef1_part_", toupper(part)), list(students))
  }

  if (toupper(part) == "E") {
    do.call(paste0("make_ef1_part_", toupper(part)), list(retention))
  }

}

