#' Set up extra_awards df for Outcome Measures part B, C, D
#'
#' @description Select correct year, ensure all award levels end up with a column
#'
#' @param df A dataframe of student statuses
#' @param award A string with the df column to use for processing depending on the OM part
#'
#' @importFrom dplyr transmute mutate filter select bind_rows n group_by summarize across everything arrange
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_to_upper
#'
#' @return A df ready for use in the make_om_part functions B-D
#'

prep_om_awards <- function(df, award) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  extra_awards <- data.frame(expand.grid(UNITID = get_ipeds_unitid(df),
                                         COHORTTYPE = c(1:4),
                                         RECIPIENT = c(1:2),
                                         AWARD = c(1:3),
                                         COUNT = 0)) %>%
                  dplyr::transmute(.data$UNITID,
                                   STUDENTID = paste0("FakeID", c(1:dplyr::n())),
                                   .data$COHORTTYPE,
                                   .data$RECIPIENT,
                                   .data$AWARD,
                                   .data$COUNT)

  award_df <- df %>%
              dplyr::transmute(.data$UNITID,
                               STUDENTID = as.character(.data$STUDENTID),
                               .data$COHORTTYPE,
                               .data$RECIPIENT,
                               AWARD = .data[[award]],
                               .data$EXCLUSION) %>%
              dplyr::mutate(COUNT = 1) %>%
              #not needed for this report
              dplyr::filter(.data$AWARD != 4,
                            .data$EXCLUSION == FALSE) %>%
              dplyr::select(-.data$EXCLUSION) %>%
              #add extras
              dplyr::bind_rows(extra_awards) %>%
              #make it wide
              tidyr::pivot_wider(names_from = .data$AWARD,
                                 values_from = .data$COUNT,
                                 values_fill = 0) %>%
              dplyr::select(-.data$STUDENTID) %>%
              #aggregate
              dplyr::group_by(.data$UNITID,
                              .data$COHORTTYPE,
                              .data$RECIPIENT)%>%
              dplyr::summarize(dplyr::across(dplyr::everything(), sum)) %>%
              dplyr::ungroup() %>%
              #sort for easy viewing
              dplyr::arrange(.data$COHORTTYPE,
                             .data$RECIPIENT)

  return(award_df)
}
