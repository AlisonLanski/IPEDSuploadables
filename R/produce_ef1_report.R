#' Shortcut function to do all steps to produce a report for Fall Enrollment
#'
#' @param students A dataframe set up according to the readme with student data
#' @param retention A dataframe set up according to the readme with retention data
#' @param part A string with what part of the report you want to produce: 'all', 'A', etc.
#' @param include_optional A boolean flag for whether optional parts should be included
#'
#' @return A txt file at the path of your choice
#' @export
#'
produce_ef1_report <- function(students, retention, part = "ALL", include_optional = FALSE) {

  students <- prep_ef1_data_frame(students)

  cip_year <- ((as.numeric(substr(Sys.Date(), 6, 7)) >= 8) + as.numeric(substr(Sys.Date(), 1, 4))) %% 2 == 1

  if (toupper(part) == "ALL") {
    make_ef1_part_A(df = students, cips = cip_year, output = "full")
    make_ef1_part_G(df = students, output = "full")

    if (cip_year == FALSE | include_optional == TRUE) {
      make_ef1_part_B(df = students, output = "full")
    }

    if (cip_year == TRUE | include_optional == TRUE) {
      make_ef1_part_C(df = students, output = "full")
    }

    make_ef1_part_D(df = students, output = "full")
    make_ef1_part_E(df = retention, output = "full")
    make_ef1_part_F(df = students, output = "full")
  }

  if(toupper(part) == "A") {
    do.call(paste0("make_ef1_part_", toupper(part)), list(students, cip_year))
  }

  if (toupper(part) %in% c("G", "B", "C", "D", "F")) {
    do.call(paste0("make_ef1_part_", toupper(part)), list(students))
  }

  if (toupper(part) == "E") {
    do.call(paste0("make_ef1_part_", toupper(part)), list(retention))
  }
}

