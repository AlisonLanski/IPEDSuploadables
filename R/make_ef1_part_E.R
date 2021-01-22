#' Make Fall Enrollment Part E
#'
#' @param df A dataframe of student/degree information
#' @param extracips A dataframe of cips offered by the institution but not in \code{'df'}
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by summarize ungroup bind_rows arrange transmute n
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'


make_ef1_part_E <- function(df, extracips = NULL, output = "part", format = "both") {


  #create the txt file
  write_report(df = partE,
               component = "FallEnrollment",
               part = "PartE",
               output = output,
               format = format)
}
