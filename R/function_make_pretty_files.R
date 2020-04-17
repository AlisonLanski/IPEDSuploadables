#' Make Pretty Files
#' @description Creates an easy to read output file.
#' 
#' @param df a data frame returned from IPEDS
#'
#' @importFrom purrr map_df
#' @importFrom stringr str_replace_all
#'
#' @examples df <- make_pretty_files(df = partA)
#' 
#' @export

make_pretty_files <- function(df = partA) {
  
  df %>%
    purrr::map_df(~stringr::str_replace_all(., "^[:upper:]+[=]*", ""))
}


