#' Make Outcome Measures Part B
#'
#' @description Award Status at Four Years after Entry
#'
#' @param df A dataframe of student statuses
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @return A text file ready for IPEDS upload
#' @export
#'
#'
make_om_part_B <- function(df, output = "part", format = "both") {

    partB <- prep_om_awards(df, award = "AwardLevel4") %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=OM1",
                     PART = "PART=B",
                     LINE = paste0("LINE=", .data$CohortType),
                     RECIPIENT_TYPE = paste0("RECIPIENT_TYPE=", .data$Recipient),
                     AWARD_CERTIFICATES = paste0("AWARD_CERTIFICATES=", .data$`1`),
                     AWARD_ASSOCIATES = paste0("AWARD_ASSOCIATES=", .data$`2`),
                     AWARD_BACHELORS = paste0("AWARD_BACHELORS=", .data$`3`)
    )

  #create the txt file
  write_report(df = partB,
               component = "OutcomeMeasures",
               part = "PartB",
               output = output,
               format = format)

}
