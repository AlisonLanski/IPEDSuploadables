#' Grab insititution's Unitid for the IPEDS script
#'
#' @return a numeric unitid
#' @param df a dataframe with ipeds data and one unitid
#'
get_ipeds_unitid <- function(df) {
  ipeds_unitid  <- as.numeric(df$Unitid[1])
  return(ipeds_unitid)
}
