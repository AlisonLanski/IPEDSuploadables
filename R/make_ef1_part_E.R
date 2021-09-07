#' Make Fall Enrollment Part E
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by summarize arrange transmute
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'


make_ef1_part_E <- function(df, output = "part", format = "both") {

  partE <- df %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=EF1",
                     PART = "PART=E",
                     FT_PY_COHORT = paste0("FT_PY_COHORT=", .data$OrigCohort[.data$IsFullTime == 1]),
                     FT_EXCLUSIONS = paste0("FT_EXCLUSIONS=", .data$Exclusions[.data$IsFullTime == 1]),
                     FT_INCLUSIONS = paste0("FT_INCLUSIONS=", .data$Inclusions[.data$IsFullTime == 1]),
                     FT_CY_COHORT = paste0("FT_CY_COHORT=", .data$StillEnrolled[.data$IsFullTime == 1]),
                     PT_PY_COHORT = paste0("PT_PY_COHORT=", .data$OrigCohort[.data$IsFullTime == 0]),
                     PT_EXCLUSIONS = paste0("PT_EXCLUSIONS=", .data$Exclusions[.data$IsFullTime == 0]),
                     PT_INCLUSIONS = paste0("PT_INCLUSIONS=", .data$Inclusions[.data$IsFullTime == 0]),
                     PT_CY_COHORT = paste0("PT_CY_COHORT=", .data$StillEnrolled[.data$IsFullTime == 0])
    ) %>%
    dplyr::distinct()

  #create the txt file
  write_report(df = partE,
               component = "FallEnrollment",
               part = "PartE",
               output = output,
               format = format)
}
