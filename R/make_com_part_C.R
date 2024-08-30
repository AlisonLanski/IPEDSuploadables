#' Make Completions Part C
#'
#' @param df A dataframe of student/degree information
#'
#' @importFrom rlang .data
#' @importFrom dplyr select group_by summarize ungroup arrange transmute n distinct
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_com_part_C <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partC <- df %>%
           dplyr::select("UNITID",
                         "STUDENTID",
                         "RACEETHNICITY",
                         "SEX") %>%
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
           dplyr::transmute(UNITID = .data$UNITID,
                            SURVSECT = "COM",
                            PART = "C",
                            RACE = .data$RACEETHNICITY,
                            SEX = .data$SEX,
                            COUNT = .data$COUNT
                            )
}
