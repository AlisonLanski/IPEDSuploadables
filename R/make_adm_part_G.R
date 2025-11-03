#' Make Admissions Part G (Transfer sex unknown)
#'
#' @description Counts the number of unknown sex for Transfer students
#'
#' @param This function relies on the student data frame that conforms to the documented specifications
#'        for the Admissions module of the IPEDS Uploadables package.
#'
#' @return Part G data prepared for inclusion in the final text file for uploading to the IPEDS portal.
#'
#' @export
#'

make_adm_part_G <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  ## Part G
  df <- filter(df, ISTRANSFER  == 1, SEX == 3 )
  partG <- df %>%
    dplyr::select(UNITID,
                  ISAPPLICANT,
                  ISADMITTED,
                  ISENROLLED,
                  ISFULLTIME,
                  SEX
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
    dplyr::summarize(COUNT = n(), .groups = "keep") %>%
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
