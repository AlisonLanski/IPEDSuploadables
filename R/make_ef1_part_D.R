#' Make Fall Enrollment Part D
#'
#' @description Count of new non-degree students
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by summarise filter arrange transmute n
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'


make_ef1_part_D <- function(df, output = "part", format = "both") {

  partD <- df %>%
    dplyr::select(.data$Unitid,
                  .data$IsDegreeCertSeeking,
                  .data$StudentLevel,
                  .data$IsFirstTime,
                  .data$IsTransfer) %>%
    dplyr::filter(.data$IsDegreeCertSeeking == 0 & .data$StudentLevel == "Undergraduate" &
                    (.data$IsFirstTime == 1 | .data$IsTransfer == 1)) %>%
    dplyr::group_by(.data$Unitid) %>%
    dplyr::summarise(Count = n()) %>%
    dplyr::ungroup() %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=EF1",
                     PART = "PART=D",
                     COUNT = paste0("COUNT=", .data$Count)
                    )

  #create the txt file
  write_report(df = partD,
               component = "FallEnrollment",
               part = "PartD",
               output = output,
               format = format)
}
