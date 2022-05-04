#' Produce an upload-compatible txt file from pre-aggregated files
#'
#' @description Use this function to create a key-value pair uploadable file
#'   from your own prepared dataframes, instead of using a different (provided)
#'   \code{produce} function. Your dataframes must be prepped to match final
#'   submission requirements as laid out by IPEDS (or whatever survey you will
#'   use this for. Use this function for one survey at a time, and add a
#'   separate dataframe for each part to the \code{...} argument. See vignette
#'   for more details.
#'
#' @note You must name the arguments for \code{survey} and \code{part} if using
#'   non-default value. If the arguments are unnamed, the function will assume
#'   their values are additional dataframes.
#'
#' @param ... dataframes (one for each survey part, in order)
#' @param survey string with the survey name you'd like in your filename
#' @param part string with the part name (subname) you'd like your file name
#'
#' @return txt file on your computer with the title
#'   \emph{[survey]_[part]_[today's date].txt}
#'
#' @export
#'
#' @examples \dontrun{
#' #With built-in R data
#' produce_other_report(mtcars[1:5,], iris[1:5,], ToothGrowth[1:5,], survey = 'FakeSurvey')
#'
#' #Will not execute properly (argument unnamed)
#' #produce_other_report(mtcars[1:5,], iris[1:5,], ToothGrowth[1:5,], 'FakeSurvey')
#' }


produce_other_report <- function(..., survey = "MySurvey", part = 'AllParts') {

  output_path <- set_report_path()

  write_report(...,
               survey = survey,
               part = part,
               output_path = output_path)

}
