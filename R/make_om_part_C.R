#' Make Outcome Measures Part C
#'
#' @description Award Status at Six Years after Entry
#'
#' @param df A dataframe of student statuses
#'
#' @importFrom dplyr transmute
#' @importFrom stringr str_to_upper
#'
#' @return A text file ready for IPEDS upload
#' @export
#'

make_om_part_C <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partC <- prep_om_awards(df, award = "AWARDLEVEL6") %>%
           #format for upload
           dplyr::transmute(UNITID = .data$UNITID,
                            SURVSECT = "OM1",
                            PART = "C",
                            LINE = .data$COHORTTYPE,
                            RECIPIENT_TYPE = .data$RECIPIENT,
                            AWARD_CERTIFICATES = .data$`1`,
                            AWARD_ASSOCIATES = .data$`2`,
                            AWARD_BACHELORS = .data$`3`)
}
