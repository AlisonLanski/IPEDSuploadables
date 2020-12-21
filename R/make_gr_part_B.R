#' Make Graduation Rates Part B
#'
#' @param df A dataframe of student/degree information
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


make_gr_part_B <- function(df, output = "part", format = "both") {

  #produce the uploadable format
  partB <- df %>%
    #aggregate the full data
    dplyr::group_by(.data$Unitid, .data$Section, .data$Line, .data$RaceEthnicity, .data$Sex) %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$Section, .data$Line, .data$RaceEthnicity, .data$Sex) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=GR1",
                     PART = "PART=B",
                     SECTION = paste0("SECTION=", .data$Section),
                     LINE = paste0("LINE=", .data$Line),
                     RACE = paste0("RACE=", .data$RaceEthnicity),
                     SEX = paste0("SEX=", .data$Sex),
                     COUNT = paste0("COUNT=", .data$Count)
                     )

  #create the txt file
  write_report(df = partB,
               component = "GradRates",
               part = "PartB",
               output = output,
               format = format)
}
