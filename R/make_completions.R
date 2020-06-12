#' Make all of Completions
#'
#' @param df A dataframe
#' @param extracips A dataframe
#'
#' @return A text file
#' @export
#'
make_completions <- function(df, extracips = NULL) {

  df <- prep_com_data_files(df = df)
  extracips <- prep_com_data_files(df = extracips)
  make_com_part_A(df = df, extracips = extracips)
  make_com_part_B(df = df, extracips = extracips)
  make_com_part_C(df = df)
  make_com_part_D(df = df, extracips = extracips)
}
