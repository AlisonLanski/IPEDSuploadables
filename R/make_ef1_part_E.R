#' Make Fall Enrollment Part E
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' 
#' @importFrom dplyr transmute distinct
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A text file
#' @export
#'

make_ef1_part_E <- function(df, output = "part", format = "both") {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partE <- df %>%
           #format for upload
           dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                            SURVSECT = "SURVSECT=EF1",
                            PART = "PART=E",
                            FT_PY_COHORT = paste0("FT_PY_COHORT=", .data$ORIGCOHORT[.data$ISFULLTIME == 1]),
                            FT_EXCLUSIONS = paste0("FT_EXCLUSIONS=", .data$EXCLUSIONS[.data$ISFULLTIME == 1]),
                            FT_INCLUSIONS = paste0("FT_INCLUSIONS=", .data$INCLUSIONS[.data$ISFULLTIME == 1]),
                            FT_CY_COHORT = paste0("FT_CY_COHORT=", .data$STILLENROLLED[.data$ISFULLTIME == 1]),
                            PT_PY_COHORT = paste0("PT_PY_COHORT=", .data$ORIGCOHORT[.data$ISFULLTIME == 0]),
                            PT_EXCLUSIONS = paste0("PT_EXCLUSIONS=", .data$EXCLUSIONS[.data$ISFULLTIME == 0]),
                            PT_INCLUSIONS = paste0("PT_INCLUSIONS=", .data$INCLUSIONS[.data$ISFULLTIME == 0]),
                            PT_CY_COHORT = paste0("PT_CY_COHORT=", .data$STILLENROLLED[.data$ISFULLTIME == 0])
           ) %>%
           dplyr::distinct()

  #create the txt file
  write_report(df = partE,
               component = "FallEnrollment",
               part = "PartE",
               output = output,
               format = format)
}
