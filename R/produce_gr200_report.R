#' Shortcut function with all steps to provide a Grad Rates 200 report
#'
#' @param df a dataframe set up according to the readme
#' @param format A string (\code{"uploadable"} will produce a properly formatted
#'   upload file. \code{"readable"} will produce a csv of the upload file (only
#'   works for one part at a time). \code{"both"} will provide both options, but
#'   only works with one part at a time.
#'
#' @return A txt or csv file at the path of your choice
#' @export
#'

produce_gr200_report <- function(df, format = "uploadable") {

  stopifnot(toupper(format) %in% c('UPLOADABLE', 'READABLE', 'BOTH'))

  survey <- 'GR200'
  part <- 'AllParts'
  output_path <- set_report_path()

  if(toupper(format) %in% c('UPLOADABLE', 'BOTH')){
    write_report(
      make_gr200(df),
      survey = survey,
      part = part,
      output_path = output_path
    )
  }

  if(toupper(format) %in% c('READABLE', 'BOTH')){
    write_report_csv(
      make_gr200(df),
      survey = survey,
      part = part,
      output_path = output_path
    )
  }


}
