#' Grad Rates 200 wrapper function to produce report
#'
#' @param df a dataframe set up according to the readme
#' @param part a string with what part of the report you want to produce "all", "A1", etc.
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @return a txt file at the path of your choice
#' @export
#'

produce_gr200_report <- function(df, part = "all", output = "full", format = "uploadable") {

    make_gr200(df, output = output, format = format)


}
