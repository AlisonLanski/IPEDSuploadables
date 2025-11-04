#' Make Admissions Part B (First-time student demographics)
#'
#' @description Aggregates First-Time student applicants by sex, race/ethnicity.
#'
#' @param This function relies on the student data frame that conforms to the documented specifications
#'        for the Admissions module of the IPEDS Uploadables package.
#'
#' @returns Part B data prepared for inclusion in the final text file for uploading to the IPEDS portal.
#'
#' @export
#'

make_adm_part_B <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

   ## pull first-time students for part B
  df <- filter(df, ISFIRSTTIME  == 1, SEX != 3 )
  partB <- df %>%
    dplyr::select(UNITID,
                  SEX,
                  RACEETHNICITY,
                  ISAPPLICANT,
                  ISADMITTED,
                  ISENROLLED,
                  ISFULLTIME,
                  ISFIRSTTIME
    ) %>%
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
                     PART = "B",
                     LINE = .data$LINE,
                     RACE = .data$RACEETHNICITY,
                     SEX = .data$SEX,
                     COUNT = .data$COUNT
    )

  return(partB)
}
