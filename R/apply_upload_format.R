#' Shortcut function to turn a dataframe into key-value pairs
#'
#' @param df dataframe with upload-compatible column names in upload-compatible order
#'
#' @return a dataframe with one column and upload-compatible rows
#'

apply_upload_format <- function(df) {

  # turn colnames into a named vector first;
  # this removes a warning message from map2_dfc
  # do NOT want to set colnames toupper here,
  #   because survey might want different caps method
  #   set it by survey (this is on the user for "other" surveys)
  stats::setNames(colnames(df), colnames(df)) %>%
  purrr::map2_dfc(df, paste, sep = "=") %>%
    tidyr::unite(col = "allunited", sep = ',')
}
