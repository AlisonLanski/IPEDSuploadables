#' Shortcut function to do all steps to produce a report
#'
#' @param df a dataframe set up according to the readme
#' @param part a string with what part of the report you want to produce "all", "A1", etc.
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @return a txt file at the path of your choice
#' @export
#'

produce_hr_report <- function(df, part = "all", output = "full", format = "uploadable") {

  df <- prep_hr_data_frame(df)

  if(toupper(part) == "ALL") {
    make_hr_part_A1(df, output = output, format = format)
    make_hr_part_A2(df, output = output, format = format)
    make_hr_part_B1(df, output = output, format = format)
    make_hr_part_B2(df, output = output, format = format)
    make_hr_part_B3(df, output = output, format = format)
    make_hr_part_D1(df, output = output, format = format)
    make_hr_part_D2(df, output = output, format = format)
    make_hr_part_D3(df, output = output, format = format)
    make_hr_part_D4(df, output = output, format = format)
    make_hr_part_G1(df, output = output, format = format)
    make_hr_part_G2(df, output = output, format = format)
    make_hr_part_H1(df, output = output, format = format)
    make_hr_part_H2(df, output = output, format = format)
  }

  if(part != "all") {
    do.call(paste0("make_hr_part_", toupper(part)), list(df))
  }
}
