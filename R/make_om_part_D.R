#' Make Outcome Measures Part D
#'
#' @description Award Status and Enrollment at Eight Years after Entry
#'
#' @param df A dataframe of student statuses
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @return A text file ready for IPEDS upload
#' @export
#'
#'
make_om_part_D <- function(df, output = "part", format = "both") {

  partD_1 <- prep_om_awards(df, award = "AwardLevel8")

  partD_2 <- df %>%
            dplyr::filter(.data$AwardLevel8 == 4,
                          .data$Exclusion == FALSE) %>%
            dplyr::mutate(Unitid = as.character(.data$Unitid),
                          Enroll = dplyr::recode(.data$EnrollStatus8,
                                                `1` = 'Enrolled',
                                                `2` = 'Elsewhere',
                                                .default = 'Error',
                                                .missing = 'Error'),
                                                Count = 1) %>%
            dplyr::select(.data$Unitid,
                          .data$StudentId,
                          .data$CohortType,
                          .data$Recipient,
                          .data$Enroll,
                          .data$Count) %>%
            tidyr::pivot_wider(names_from = .data$Enroll,
                               values_from = .data$Count,
                               values_fill = 0) %>%
            dplyr::select(-.data$StudentId) %>%
            #aggregate
            dplyr::group_by(.data$Unitid, .data$CohortType, .data$Recipient)%>%
            dplyr::summarize(dplyr::across(dplyr::everything(), sum)) %>%
            dplyr::ungroup()

  partD <- dplyr::full_join(partD_1, partD_2,
                              by = c("Unitid", "CohortType", "Recipient")) %>%
            dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 0))) %>%
            #format for upload
            dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                             SURVSECT = "SURVSECT=OM1",
                             PART = "PART=D",
                             LINE = paste0("LINE=", .data$CohortType),
                             RECIPIENT_TYPE = paste0("RECIPIENT_TYPE=", .data$Recipient),
                             AWARD_CERTIFICATES = paste0("AWARD_CERTIFICATES=", .data$`1`),
                             AWARD_ASSOCIATES = paste0("AWARD_ASSOCIATES=", .data$`2`),
                             AWARD_BACHELORS = paste0("AWARD_BACHELORS=", .data$`3`),
                             STILL_ENROLLED = paste0("STILL_ENROLLED=", .data$Enrolled),
                             ENROLLED_ANOTHER = paste0("ENROLLED_ANOTHER=", .data$Elsewhere)
                             )

  #create the txt file
  write_report(df = partD,
               component = "OutcomeMeasures",
               part = "PartD",
               output = output,
               format = format)
}
