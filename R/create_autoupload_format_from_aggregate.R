#' Shortcut function to autoformat any file that's already aggregated.
#' Columns must already be in correct name format.
#'
#' @param df A dataframe
#' @param output_file_name A string containing a file name for your results
#'
#' @importFrom purrr map2_dfc
#' @importFrom tidyr unite
#' @importFrom stringr str_to_upper
#'
#' @return A txt file at the path of your choice
#' @export
#'
create_autoupload_format_from_aggregate <- function(df, output_file_name) {

  while (!exists(x = "output_path", envir = globalenv())) {
    set_report_path()
  }

  colnames(df) <- stringr::str_to_upper(colnames(df))

  purrr::map2_dfc(colnames(df), df, paste, sep = "=") %>%
  tidyr::unite(col = "allunited", sep = ',') %>%
  write.table(sep = ",",
              file = paste0(output_path, output_file_name, "_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)
}
