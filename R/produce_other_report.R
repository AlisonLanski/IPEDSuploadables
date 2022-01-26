#' Produce an upload-compatible txt file from pre-aggregated files
#'
#' @description Use this function to create a key-value pair uploadable file
#'   from your own prepared dataframes, instead of using a different (provided)
#'   \code{produce} function. Your dataframes must be prepped to match final
#'   submission requirements as laid out by IPEDS. Use this function for one survey at a time,
#'   and add a separate dataframe for each part to the \code{...} argument. See
#'   vignette for more details.
#'
#' @note This function can be used to produce any key-value pair txt file for
#'   upload (IPEDS or non-IPEDS) as long as the initial dataframes are correctly
#'   formatted.
#' @param ... dataframes (one for each survey part, in order)
#' @param survey string with the survey name you'd like in your filename
#' @param part string with the parts included by your dataframe
#'
#' @return txt file on your computer with the title \emph{[survey]_[part]_[today's date].txt}
#'
#' @export
#'
#' @examples \dontrun{
#' #With built-in R data
#' produce_other_report(mtcars[1:5,], iris[1:5,], ToothGrowth[1:5,], survey = 'FakeSurvey')}


produce_other_report <- function(..., survey, part = 'AllParts') {
  dfs <- list(...)
  purrr::map_df(dfs,
                apply_upload_format) %>%
    write_report(component = survey,
                 part = part,
                 output = 'PART',
                 append = FALSE,
                 format = 'uploadable'
    )
}
