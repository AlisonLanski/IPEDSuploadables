#' Write the prepared data to a txt file in key-value format
#'
#' @note All arguments for this function are required and must be named.
#'   Dataframes must have the key as the column name (with appropriate
#'   capitalization) and the value in the cells
#'
#' @param ... dataframes (one for each survey part, in order)
#' @param survey a string (which [IPEDS] survey)
#' @param part a string (which upload part of the survey)
#' @param output_path a file path (where the file should be saved)
#'
#' @importFrom utils write.table
#' @importFrom purrr compact map_df
#' @importFrom stringr str_remove_all
#'
#' @return a txt file (at the path location)
#' @export
#'

write_report <- function(..., survey, part, output_path) {

  fullpath <- paste0(output_path, survey, "_", part, "_", Sys.Date(), ".txt")

  dfs <- list(...)

  #remove any empty dfs (can happen from fall enrollment, maybe others)
  purrr::compact(dfs) %>%

    purrr::map_df(apply_upload_format) %>%

    write.table(sep = ",",
                file = fullpath,
                quote = FALSE, row.names = FALSE, col.names = FALSE)

  cat("Uploadable results available at", fullpath)

}


