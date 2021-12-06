#' Make Outcome Measures Part C
#'
#' @description Award Status at Six Years after Entry
#'
#' @param df A dataframe of student statuses
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom dplyr transmute
#' @importFrom stringr str_to_upper
#'
#' @return A text file ready for IPEDS upload
#' @export
#'

make_om_part_C <- function(df, output = "part", format = "both") {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partC <- prep_om_awards(df, award = "AWARDLEVEL6") %>%
           #format for upload
           dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                            SURVSECT = "SURVSECT=OM1",
                            PART = "PART=C",
                            LINE = paste0("LINE=", .data$COHORTTYPE),
                            RECIPIENT_TYPE = paste0("RECIPIENT_TYPE=", .data$RECIPIENT),
                            AWARD_CERTIFICATES = paste0("AWARD_CERTIFICATES=", .data$`1`),
                            AWARD_ASSOCIATES = paste0("AWARD_ASSOCIATES=", .data$`2`),
                            AWARD_BACHELORS = paste0("AWARD_BACHELORS=", .data$`3`)
           )

  #create the txt file
  write_report(df = partC,
               component = "OutcomeMeasures",
               part = "PartC",
               output = output,
               format = format)
}
