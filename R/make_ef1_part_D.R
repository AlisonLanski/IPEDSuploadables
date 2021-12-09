#' Make Fall Enrollment Part D
#'
#' @description COUNT of new non-degree students
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' 
#' @importFrom dplyr select group_by summarise filter arrange transmute n
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A text file
#' @export
#'

make_ef1_part_D <- function(df, output = "part", format = "both") {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partD <- df %>%
           dplyr::select(.data$UNITID,
                         .data$ISDEGREECERTSEEKING,
                         .data$STUDENTLEVEL,
                         .data$ISFIRSTTIME,
                         .data$ISTRANSFER) %>%
           dplyr::filter(.data$ISDEGREECERTSEEKING == 0 & .data$STUDENTLEVEL == "Undergraduate" &
                           (.data$ISFIRSTTIME == 1 | .data$ISTRANSFER == 1)) %>%
           dplyr::group_by(.data$UNITID) %>%
           dplyr::summarise(COUNT = n()) %>%
           dplyr::ungroup() %>%
           #format for upload
           dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                            SURVSECT = "SURVSECT=EF1",
                            PART = "PART=D",
                            COUNT = paste0("COUNT=", .data$COUNT)
                           )

  #create the txt file
  write_report(df = partD,
               component = "FallEnrollment",
               part = "PartD",
               output = output,
               format = format)
}
