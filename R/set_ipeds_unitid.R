#' Establish your insititution's Unitid for the IPEDS script
#'
#' @return a numeric unitid
#' @export
#' @importFrom svDialogs dlgInput
#'
set_ipeds_unitid <- function() {
  ipeds_unitid  <- as.numeric(svDialogs::dlgInput("What is your school's IPEDS Unitid?",111111)$res)
  return(ipeds_unitid)
}
