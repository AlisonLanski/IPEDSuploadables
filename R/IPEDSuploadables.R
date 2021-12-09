#' \code{IPEDSuploadables} package
#'
#' Tools to assist uploading survey data to IPEDS
#'
#'
#' @docType package
#' @name IPEDSuploadables
#' @importFrom magrittr %>%

NULL

## quiets concerns of R CMD check re: output path; or at least it used to
if(getRversion() >= "2.15.1")  utils::globalVariables(c('output_path'))


# see this for an example of a more complete file:
# https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
