#' Make Outcome Measures Part A
#'
#' @description Establishing the Outcome Measures cohorts
#'
#' @param df A dataframe of student statuses
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom dplyr group_by summarize ungroup arrange transmute n
#' @importFrom stringr str_to_upper
#'
#' @return A text file ready for IPEDS upload
#' @export
#'

make_om_part_A <- function(df, output = "part", format = "both") {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partA <- df %>%
          #aggregate the full data
          dplyr::group_by(.data$UNITID,
                          .data$COHORTTYPE,
                          .data$RECIPIENT)%>%
          dplyr::summarize(COHORTCOUNT = dplyr::n(),
                           EXCLUSIONCOUNT = sum(.data$EXCLUSION)) %>%
          dplyr::ungroup() %>%
          #sort for easy viewing
          dplyr::arrange(.data$COHORTTYPE,
                         .data$RECIPIENT) %>%
          #format for upload
          dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                           SURVSECT = "SURVSECT=OM1",
                           PART = "PART=A",
                           LINE = paste0("LINE=", .data$COHORTTYPE),
                           RECIPIENT_TYPE = paste0("RECIPIENT_TYPE=", .data$RECIPIENT),
                           COHORT = paste0("COHORT=", .data$COHORTCOUNT),
                           EXCLUSION = paste0("EXCLUSION=", .data$EXCLUSIONCOUNT)
          )

  #create the txt file
  write_report(df = partA,
               component = "OutcomeMeasures",
               part = "PartA",
               output = output,
               format = format)
}
