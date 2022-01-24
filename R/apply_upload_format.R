#' Shortcut function to turn a dataframe into key-value pairs
#'
#' @param df dataframe with upload-compatible column names in upload-compatible order
#'
#' @return a dataframe with one column and upload-compatible rows
#'

apply_upload_format <- function(df) {

  while (!exists(x = "output_path", envir = globalenv())) {
    set_report_path()
  }

  colnames(df) <- stringr::str_to_upper(colnames(df))

  purrr::map2_dfc(colnames(df), df, paste, sep = "=") %>%
    tidyr::unite(col = "allunited", sep = ',')
}
