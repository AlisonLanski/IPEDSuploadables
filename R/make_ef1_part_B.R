#' Make Fall Enrollment Part B
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


make_ef1_part_B <- function(df, extracips = NULL, output = "part", format = "both") {

  partB <- partB %>%
    #sort for easy viewing
    dplyr::arrange(.data$Line, .data$Level, .data$Sex) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=EF1",
                     PART = "PART=B",
                     LINE = paste0("LINE=", .data$Line),
                     SLEVEL = paste0("RACE=", .data$Level),
                     SEX = paste0("SEX=", .data$Sex),
                     COUNT = paste0("COUNT=", .data$Count)
    )

  #create the txt file
  write_report(df = partB,
               component = "FallEnrollment",
               part = "PartB",
               output = output,
               format = format)
}
