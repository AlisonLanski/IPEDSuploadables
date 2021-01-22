#' Make Fall Enrollment Part A
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


make_ef1_part_A <- function(df, extracips = NULL, output = "part", format = "both") {

  partA <- partA %>%
    #sort for easy viewing
    dplyr::arrange(.data$MajorCip, .data$Line, .data$RaceEthnicity, .data$Sex) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=EF1",
                     PART = "PART=A",
                     CIPCODE = paste0("CIPCODE=", .data$MajorCip),
                     LINE = paste0("LINE=", .data$Line),
                     RACE = paste0("RACE=", .data$RaceEthnicity),
                     SEX = paste0("SEX=", .data$Sex),
                     COUNT = paste0("COUNT=", .data$Count)
    )

  #create the txt file
  write_report(df = partA,
               component = "FallEnrollment",
               part = "PartA",
               output = output,
               format = format)
}
