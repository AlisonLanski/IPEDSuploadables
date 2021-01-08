#' Prep df for Outcome Measures part B, C, D
#'
#' @description Select correct year, ensure all award levels end up with a column
#'
#' @param df A dataframe of student statuses
#' @param award A string with the df column to use for processing
#'
#' @return A df ready for upload formatting
#' @export
#'
#' @examples
#'
prep_om_awards <- function(df, award) {

  extra_awards <- data.frame(expand.grid(Unitid = 999999,#get_ipeds_unitid(om_dat),
                                         CohortType = c(1:4),
                                         Recipient = c(1:2),
                                         Award = c(1:3),
                                         Count = 0)) %>%
    dplyr::transmute(Unitid,
                     StudentId = paste0("FakeID", c(1:n())),
                     CohortType,
                     Recipient,
                     Award,
                     Count)

  #partB <- df %>%

  award_df <- df %>%
    dplyr::transmute(.data$Unitid,
                  .data$StudentId,
                  .data$CohortType,
                  .data$Recipient,
                  Award = .data[[award]]) %>%
    dplyr::mutate(Count = 1) %>%

    #not needed for this report
    dplyr::filter(.data$Award != 4,
                  .data$Exclusion == FALSE) %>%

    #add extras
    rbind(extra_awards) %>%

    #make it wide
    tidyr::pivot_wider(names_from = .data$Award, values_from = .data$Count, values_fill = 0) %>%
    dplyr::select(-.data$StudentId) %>%

    #aggregate
    dplyr::group_by(.data$Unitid, .data$CohortType, .data$Recipient)%>%
    dplyr::summarize(dplyr::across(dplyr::everything(), sum)) %>%
    dplyr::ungroup() %>%

    #sort for easy viewing
    dplyr::arrange(.data$CohortType, .data$Recipient) %>%

    #remove empty rows
    dplyr::filter(!(`1`==0 & `2`==0 & `3`==0))

  return(award_df)

}
