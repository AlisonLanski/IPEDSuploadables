#' Shortcut function to do all steps to produce a report
#'
#' @param df a dataframe set up according to the readme
#' @param part a string with what part of the report you want to produce "all", "A1", etc.
#'
#' @return a txt file at the path of your choice
#' @export
#'

produce_gr_report <- function(df, part = "all"){

  if(toupper(part) == "ALL") {
    make_gr_part_B(df, output = "full")
    make_gr_part_C(df, output = "full")
  }

  if(part != "all") {
    do.call(paste0("make_gr_part_", toupper(part)), list(df))
  }
}
