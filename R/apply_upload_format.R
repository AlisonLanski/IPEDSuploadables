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

  # turn colnames into s named vector first;
  # this removes a warning message from map2_dfc
  # might as well do upper at the same time
  stats::setNames(toupper(colnames(df)), colnames(df)) %>%
  purrr::map2_dfc(df, paste, sep = "=") %>%
    tidyr::unite(col = "allunited", sep = ',')
}
