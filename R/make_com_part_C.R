#' Make Completions Part C
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom dplyr select group_by summarize ungroup arrange transmute n distinct
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A text file
#' @export
#'

make_com_part_C <- function(df, output = "part", format = "both") {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partC <- df %>%
           dplyr::select(.data$UNITID,
                         .data$STUDENTID,
                         .data$RACEETHNICITY,
                         .data$SEX) %>%
           #deduplicate
           dplyr::distinct() %>%
           #aggregate and count
           dplyr::group_by(.data$UNITID,
                           .data$RACEETHNICITY,
                           .data$SEX) %>%
           dplyr::summarize(COUNT = dplyr::n()) %>%
           dplyr::ungroup() %>%
           #sort for easy viewing
           dplyr::arrange(.data$RACEETHNICITY,
                          .data$SEX) %>%
           #format for upload
           dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                            SURVSECT = "SURVSECT=COM",
                            PART = "PART=C",
                            RACE = paste0("RACE=", .data$RACEETHNICITY),
                            SEX = paste0("SEX=", .data$SEX),
                            COUNT = paste0("COUNT=", .data$COUNT)
                            )

  write_report(df = partC,
               component = "Completions",
               part = "PartC",
               output = output,
               format = format)
}
