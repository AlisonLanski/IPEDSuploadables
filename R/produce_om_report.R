#' Shortcut function to do all steps to produce a report
#'
#' @param df A dataframe set up according to the readme
#' @param part A string with what part of the report you want to produce: 'all', 'A', etc.
#'
#' @return A txt file at the path of your choice
#' @export
#'

produce_om_report <- function(df, part = "ALL") {

   students <- prep_om_data_frame(df = df)

  if (toupper(part) == "ALL") {
    make_om_part_A(df = students, output = "full")
    make_om_part_B(df = students, output = "full")
    make_om_part_C(df = students, output = "full")
    make_om_part_D(df = students, output = "full")
  }

  if (toupper(part) %in% c("A", "B", "C", "D")) {
    do.call(paste0("make_om_part_", toupper(part)), list(students))
  }
}

