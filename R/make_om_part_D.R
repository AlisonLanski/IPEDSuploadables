#' Make Outcome Measures Part D
#'
#' @description Award Status and ENROLLment at Eight Years after Entry
#'
#' @param df A dataframe of student statuses
#'
#' @importFrom dplyr filter mutate select group_by summarize full_join ungroup across everything
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom stringr str_to_upper
#'
#' @return A text file ready for IPEDS upload
#' @export
#'

make_om_part_D <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #standard awards counting
  partD_1 <- prep_om_awards(df, award = "AWARDLEVEL8")


  #### What happened to everyone else?
  #d2 dummy: need all columns after pivot; counts set to 0 to avoid changing totals
  d2_dummy <- data.frame(
    tidyr::expand_grid(UNITID = get_ipeds_unitid(df),
                COHORTTYPE = c(1:4),
                RECIPIENT = c(1:2),
                ENROLLED = 0,
                ELSEWHERE = 0)
  )

  #look at other people; add dummy to avoid missing columns for pivot errors
  partD_2 <- df %>%
    dplyr::filter(.data$AWARDLEVEL8 == 4,
                  .data$EXCLUSION == FALSE) %>%
    dplyr::mutate(UNITID = as.character(.data$UNITID),
                  ENROLL = dplyr::recode(.data$ENROLLSTATUS8,
                                         `1` = "ENROLLED",
                                         `2` = "ELSEWHERE",
                                         .default = "Extra",
                                         .missing = "Extra"),
                  COUNT = 1) %>%
    dplyr::select(.data$UNITID,
                  .data$STUDENTID,
                  .data$COHORTTYPE,
                  .data$RECIPIENT,
                  .data$ENROLL,
                  .data$COUNT) %>%
    tidyr::pivot_wider(names_from = .data$ENROLL,
                       values_from = .data$COUNT,
                       values_fill = 0) %>%

    #add empty rows for completeness
    bind_rows(d2_dummy) %>%

    dplyr::select(.data$UNITID,
                  .data$COHORTTYPE,
                  .data$RECIPIENT,
                  .data$ENROLLED,
                  .data$ELSEWHERE) %>%

    #aggregate
    dplyr::group_by(.data$UNITID,
                    .data$COHORTTYPE,
                    .data$RECIPIENT)%>%
    dplyr::summarize(dplyr::across(dplyr::everything(),
                                   sum, na.rm = TRUE)) %>%
    dplyr::ungroup()

  partD <- dplyr::full_join(partD_1, partD_2,
                            by = c("UNITID", "COHORTTYPE", "RECIPIENT")) %>%
    #verify column order to prep for across
    dplyr::select(.data$UNITID,
                  .data$COHORTTYPE,
                  .data$RECIPIENT,
                  dplyr::everything()) %>%
    #fix any NAs from the join -- pick only the numerics we're checking
    dplyr::mutate(dplyr::across(c(4:8),
                                ~tidyr::replace_na(.x, 0))) %>%
    #format for upload
    dplyr::transmute(UNITID = .data$UNITID,
                     SURVSECT = "OM1",
                     PART = "D",
                     LINE = .data$COHORTTYPE,
                     RECIPIENT_TYPE = .data$RECIPIENT,
                     AWARD_CERTIFICATES = .data$`1`,
                     AWARD_ASSOCIATES = .data$`2`,
                     AWARD_BACHELORS = .data$`3`,
                     STILL_ENROLLED = .data$ENROLLED,
                     ENROLLED_ANOTHER = .data$ELSEWHERE)

}
