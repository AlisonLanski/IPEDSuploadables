#' Make Admissions Part F (Transfer student demographics)
#'
#' @description Aggregates Transfer student applicants by sex, race/ethnicity.
#'
#' @param This function relies on the student data frame that conforms to the documented specifications
#'        for the Admissions module of the IPEDS Uploadables package.
#'
#' @returns Part F data prepared for inclusion in the final text file for uploading to the IPEDS portal.
#'
#' @export
#'


make_admin_part_F <- function(df) {
# Pull transfer students for part F

  colnames(df) <- stringr::str_to_upper(colnames(df))

  df <- filter(df, ISTRANSFER == 1, SEX != 3)
  partF <- df %>%
    dplyr::select(UNITID,
                  SEX,
                  RACEETHNICITY,
                  ISAPPLICANT,
                  ISADMITTED,
                  ISENROLLED,
                  ISFULLTIME,
                  ISTRANSFER) %>%
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
    dplyr::summarize(COUNT = n(), .groups = "keep") %>%
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
