#' Make Completions Part C
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})#'
#' @importFrom rlang .data
#' @importFrom dplyr select group_by summarize ungroup arrange transmute n
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'

make_com_part_C <- function(df, output = "part", format = "both") {

  partC <- df %>%
    dplyr::select(.data$Unitid, .data$StudentId, .data$RaceEthnicity, .data$Sex) %>%
    #deduplicate
    unique() %>%
    #aggregate and count
    dplyr::group_by(.data$Unitid, .data$RaceEthnicity, .data$Sex) %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$RaceEthnicity, .data$Sex) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=COM",
                     PART = "PART=C",
                     RACE = paste0("RACE=", .data$RaceEthnicity),
                     SEX = paste0("SEX=", .data$Sex),
                     COUNT = paste0("COUNT=", .data$Count)
                     )

  write_report(df = partC,
               component = 'Completions',
               part = "PartC",
               output = output,
               format = format)
}
