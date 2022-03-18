#' Write the prepared data to a csv file
#'
#' @note All arguments for this function are required.
#'   The dataframe must have the key as the column name (with appropriate
#'   capitalization) and the value in the cells
#'
#' @param df a dataframe (prepared via the 'make' scripts)
#' @param survey a string (which IPEDS survey)
#' @param part a string (which upload part of the survey)
#' @param output_path a path (which folder the report should go in)
#'
#' @importFrom utils write.table
#'
#' @return a csv file (at the path location)
#' @export
#'

write_report_csv <- function(df, survey, part, output_path) {

  fullpath <- paste0(output_path, survey, "_", part, "_Readable_",  Sys.Date(), ".csv")

  df %>%
    write.table(sep = ",",
                file = fullpath,
                quote = FALSE, row.names = FALSE, col.names = TRUE, na = '')

  cat("Readable results available at ", fullpath)
}


