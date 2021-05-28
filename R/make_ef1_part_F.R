#' Make Fall Enrollment Part F
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by summarize arrange transmute n
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'


make_ef1_part_F <- function(df, output = "part", format = "both") {

  partF <- df %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=EF1",
                     PART = "PART=F",
                     ST_STAFF_RATIO = paste0("ST_STAFF_RATIO=", .data$StaffRatio)
    )

  #create the txt file
  write_report(df = partF,
               component = "FallEnrollment",
               part = "PartF",
               output = output,
               format = format)
}
