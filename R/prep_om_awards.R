#' Prep extra_awards df for Outcome Measures part B, C, D
#'
#' @description Select correct year, ensure all award levels end up with a column
#'
#' @param df A dataframe of student statuses
#' @param award A string with the df column to use for processing depending on the OM part
#'
#' @return A df ready for use in the make_om_part functions B-D
#'
#'
prep_om_awards <- function(df, award) {

  extra_awards <- data.frame(expand.grid(Unitid = get_ipeds_unitid(df),
                                         CohortType = c(1:4),
                                         Recipient = c(1:2),
                                         Award = c(1:3),
                                         Count = 0)) %>%
    dplyr::transmute(.data$Unitid,
                     StudentId = paste0("FakeID", c(1:dplyr::n())),
                     .data$CohortType,
                     .data$Recipient,
                     .data$Award,
                     .data$Count)

  #partB <- df %>%

  award_df <- df %>%
    dplyr::transmute(.data$Unitid,
                  StudentId = as.character(.data$StudentId),
                  .data$CohortType,
                  .data$Recipient,
                  Award = .data[[award]],
                  .data$Exclusion) %>%
    dplyr::mutate(Count = 1) %>%

    #not needed for this report
    dplyr::filter(.data$Award != 4,
                  .data$Exclusion == FALSE) %>%
    dplyr::select(-.data$Exclusion) %>%

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
    dplyr::filter(!(.data$`1`==0 & .data$`2`==0 & .data$`3`==0))


  return(award_df)

}
