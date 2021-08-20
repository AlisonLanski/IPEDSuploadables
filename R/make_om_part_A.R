#' Make Outcome Measures Part A
#'
#' @description Establishing the Outcome Measures cohorts
#'
#' @param df A dataframe of student statuses
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @return A text file ready for IPEDS upload
#' @export
#'
#'
make_om_part_A <- function(df, output = "part", format = "both") {

  partA <- df %>%
          #aggregate the full data
          dplyr::group_by(.data$Unitid, .data$CohortType, .data$Recipient)%>%
          dplyr::summarize(CohortCount = dplyr::n(),
                           ExclusionCount = sum(.data$Exclusion)) %>%
          dplyr::ungroup() %>%
          #sort for easy viewing
          dplyr::arrange(.data$CohortType, .data$Recipient) %>%
          #format for upload
          dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                           SURVSECT = "SURVSECT=OM1",
                           PART = "PART=A",
                           LINE = paste0("LINE=", .data$CohortType),
                           RECIPIENT_TYPE = paste0("RECIPIENT_TYPE=", .data$Recipient),
                           COHORT = paste0("COHORT=", .data$CohortCount),
                           EXCLUSION = paste0("EXCLUSION=", .data$ExclusionCount)
          )

  #create the txt file
  write_report(df = partA,
               component = "OutcomeMeasures",
               part = "PartA",
               output = output,
               format = format)
}
