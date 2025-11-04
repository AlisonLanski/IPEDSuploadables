#' Make Admissions Part F (Transfer student demographics)
#'
#' @description Aggregates Transfer student applicants by sex, race/ethnicity.
#'
#' @param df A dataframe of applicant information
#'
#' @return Admissions Part F data with the required IPEDS structure
#'
#' @export
#'


make_adm_part_F <- function(df) {
# Pull transfer students for part F

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partF <- df %>%
    dplyr::filter(.data$ISTRANSFER == 1,
                  .data$SEX != 3) %>%
    dplyr::select("UNITID",
                  "SEX",
                  "RACEETHNICITY",
                  "ISAPPLICANT",
                  "ISADMITTED",
                  "ISENROLLED",
                  "ISFULLTIME",
                  "ISTRANSFER") %>%
    dplyr::mutate(LINE = dplyr::case_when(
                                .data$ISADMITTED == 0 ~ 1,
                                .data$ISENROLLED == 0 ~ 2,
                                .data$ISENROLLED == 1 & .data$ISFULLTIME == 1 ~ 3,
                                .data$ISENROLLED == 1 & .data$ISFULLTIME == 0 ~ 4
    )) %>%
    dplyr::group_by(.data$UNITID,
                  .data$LINE,
                  .data$RACEETHNICITY,
                  .data$SEX,
    ) %>%
    dplyr::summarize(COUNT = n()) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(UNITID = .data$UNITID,
                   SURVSECT = "ADM",
                   PART = "F",
                   LINE = .data$LINE,
                   RACE = .data$RACEETHNICITY,
                   SEX = .data$SEX,
                   COUNT = .data$COUNT
    )

  return(partF)

}
