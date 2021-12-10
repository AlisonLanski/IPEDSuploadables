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

produce_gr_report <- function(df, part = "all", output = "full", format = "uploadable") {

  if(toupper(part) == "ALL") {
    make_gr_part_B(df, output = output, format = format)
    make_gr_part_C(df, output = output, format = format)
  }

  if(part != "all") {
    do.call(paste0("make_gr_part_", toupper(part)), list(df, output, format))
  }
}
