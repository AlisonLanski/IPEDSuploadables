#' Make Fall Enrollment Part E
#'
#' @description Retention counts
#' @param df A dataframe of student/degree information
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

make_ef1_part_E <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partE <- df %>%
           #format for upload
           dplyr::transmute(UNITID = .data$UNITID,
                            SURVSECT = "EF1",
                            PART = "E",
                            FT_PY_COHORT = .data$ORIGCOHORT[.data$ISFULLTIME == 1],
                            FT_EXCLUSIONS = .data$EXCLUSIONS[.data$ISFULLTIME == 1],
                            FT_INCLUSIONS =  .data$INCLUSIONS[.data$ISFULLTIME == 1],
                            FT_CY_COHORT = .data$STILLENROLLED[.data$ISFULLTIME == 1],
                            PT_PY_COHORT = .data$ORIGCOHORT[.data$ISFULLTIME == 0],
                            PT_EXCLUSIONS = .data$EXCLUSIONS[.data$ISFULLTIME == 0],
                            PT_INCLUSIONS = .data$INCLUSIONS[.data$ISFULLTIME == 0],
                            PT_CY_COHORT = .data$STILLENROLLED[.data$ISFULLTIME == 0]
           ) %>%
           dplyr::distinct()
}
