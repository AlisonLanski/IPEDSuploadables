#' Make Admissions Part G (Transfer sex unknown)
#'
#' @description Counts the number of unknown sex for Transfer students
#'
#' @param df A dataframe of applicant information
#'
#' @return Admissions Part G data with the required IPEDS structure
#'
#' @export
#'

make_adm_part_G <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  ## Part G
  partG <- df %>%
    dplyr::filter(.data$ISTRANSFER  == 1,
                  .data$SEX == 3) %>%
    dplyr::select("UNITID",
                  "ISAPPLICANT",
                  "ISADMITTED",
                  "ISENROLLED",
                  "ISFULLTIME",
                  "SEX"
    ) %>%
    dplyr::mutate(LINE = dplyr::case_when(
                                          .data$ISADMITTED == 0 ~ 1,
                                          .data$ISENROLLED == 0 ~ 2,
                                          .data$ISENROLLED == 1 & .data$ISFULLTIME == 1 ~ 3,
                                          .data$ISENROLLED == 0 & .data$ISFULLTIME == 0 ~ 4
    )) %>%
    dplyr::group_by(.data$UNITID,
                    .data$LINE,
                    .data$SEX
    ) %>%
    dplyr::summarize(COUNT = n()) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(UNITID = .data$UNITID,
                     SURVSECT = 'ADM',
                     PART = "G",
                     LINE = .data$LINE,
                     ADMSEX = .data$COUNT)

  if (all(is.na(df)) == TRUE) {
    return()
  } else {
    return(partG)
  }
}
