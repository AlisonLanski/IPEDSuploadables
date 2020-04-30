#' Make all of Completions
#'
#' @param df
#' @param extracips
#'
#' @return
#' @export
#'
#' @examples
make_completions <- function(df, extracips = NULL) {

  make_com_part_A(df = df, extracips = extracips)
  make_com_part_B(df = df, extracips = extracips)
  make_com_part_C(df = df)
  make_com_part_D(df = df)
}
