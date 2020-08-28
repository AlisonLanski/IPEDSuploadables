#' \code{IPEDSuploadables} package
#'
#' Tools to assist in connections to Notre Dame databases
#'
#'
#' @docType package
#' @name IPEDSuploadables

NULL

## quiets concerns of R CMD check re: the DBI outputs that we need from the connect scripts
if(getRversion() >= "2.15.1")  utils::globalVariables(c('output_path'))


# see this for an example of a more complete file:
# https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
