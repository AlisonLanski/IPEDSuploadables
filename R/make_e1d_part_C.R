#' Make 12 Month Enrollment Part C
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


make_e1d_part_C <- function(df, extracips = NULL, output = "part", format = "both") {

  partC <- partC %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=E1D",
                     PART = "PART=C",
                     LINE = paste0("LINE=", .data$Line),
                     ENROLL_EXCLUSIVE = paste0("ENROLL_EXCLUSIVE=", .data$DistanceEdOnly),
                     ENROLL_SOME = paste0("ENROLL_SOME=", .data$DistanceEdSome)
    )

  #create the txt file
  write_report(df = partC,
               component = "12MonthEnrollment",
               part = "PartC",
               output = output,
               format = format)
}
