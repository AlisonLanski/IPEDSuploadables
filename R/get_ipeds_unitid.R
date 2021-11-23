#' Grab institution's UNITID for the IPEDS script
#'
#' @param df a dataframe with ipeds data and one unitid
#'
#' @importFrom stringr str_to_upper
#'
#' @return a numeric unitid
#' @export
#'

get_ipeds_unitid <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))
  ipeds_unitid  <- as.character(df$UNITID[1])

  return(ipeds_unitid)
}
