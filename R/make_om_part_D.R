#' Make Outcome Measures Part D
#'
#' @description Award Status and ENROLLment at Eight Years after Entry
#'
#' @param df A dataframe of student statuses
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom dplyr filter mutate select group_by summarize full_join ungroup across everything
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom stringr str_to_upper
#'
#' @return A text file ready for IPEDS upload
#' @export
#'

make_om_part_D <- function(df, output = "part", format = "both") {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partD_1 <- prep_om_awards(df, award = "AWARDLEVEL8")

  partD_2 <- df %>%
             dplyr::filter(.data$AWARDLEVEL8 == 4,
                           .data$EXCLUSION == FALSE) %>%
             dplyr::mutate(UNITID = as.character(.data$UNITID),
                           ENROLL = dplyr::recode(.data$ENROLLSTATUS8,
                                                 `1` = "ENROLLED",
                                                 `2` = "ELSEWHERE",
                                                 .default = "Error",
                                                 .missing = "Error"),
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
             dplyr::select(-.data$STUDENTID) %>%
             #aggregate
             dplyr::group_by(.data$UNITID,
                             .data$COHORTTYPE,
                             .data$RECIPIENT)%>%
             dplyr::summarize(dplyr::across(dplyr::everything(),
                                            sum)) %>%
             dplyr::ungroup()

  partD <- dplyr::full_join(partD_1, partD_2,
                              by = c("UNITID", "COHORTTYPE", "RECIPIENT")) %>%
           dplyr::mutate(dplyr::across(dplyr::everything(),
                                       ~tidyr::replace_na(.x, 0))) %>%
           #format for upload
           dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                            SURVSECT = "SURVSECT=OM1",
                            PART = "PART=D",
                            LINE = paste0("LINE=", .data$COHORTTYPE),
                            RECIPIENT_TYPE = paste0("RECIPIENT_TYPE=", .data$RECIPIENT),
                            AWARD_CERTIFICATES = paste0("AWARD_CERTIFICATES=", .data$`1`),
                            AWARD_ASSOCIATES = paste0("AWARD_ASSOCIATES=", .data$`2`),
                            AWARD_BACHELORS = paste0("AWARD_BACHELORS=", .data$`3`),
                            STILL_ENROLLED = paste0("STILL_ENROLLED=", .data$ENROLLED),
                            ENROLLED_ANOTHER = paste0("ENROLLED_ANOTHER=", .data$ELSEWHERE)
                            )

  #create the txt file
  write_report(df = partD,
               component = "OutcomeMeasures",
               part = "PartD",
               output = output,
               format = format)
}
