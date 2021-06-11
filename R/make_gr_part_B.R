#' Make Graduation Rates Part B
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#' @param cohort_year A numerical value for the fall cohort
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by summarize ungroup bind_rows arrange transmute n
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'


make_gr_part_B <- function(df, output = "part", format = "both", cohort_year = 2013) {

  #produce the uploadable format
  partB <- df %>%
    dplyr::select(.data$Unitid,
                  .data$StudentID,
                  .data$RaceEthnicity,
                  .data$Sex,
                  .data$Cohort,
                  .data$IsDegreeSeeking,
                  .data$IsCertSeeking,
                  .data$IsFirstTime,
                  .data$IsFullTime
                  ) %>%
    dplyr::mutate(Section = case_when(
                              .data$Cohort == cohort_year ~ 1,
                              .data$Cohort == cohort_year & .data$IsFullTime == 1 & .data$IsFirstTime == 1 & .data$IsDegreeSeeking == 1 ~ 2,
                              .data$Cohort == cohort_year & .data$IsFullTime == 1 & .data$IsFirstTime == 1 & (.data$IsDegreeSeeking | .data$IsCertSeeking == 1) ~ 3
                            ),
                  Line = case_when(

                            )
                  ) %>%
    #get rid of the total line that is not needed for part B
    dplyr::filter(.data$Line != 29) %>%
    #aggregate the full data
    dplyr::group_by(.data$Unitid, .data$Section, .data$Line, .data$RaceEthnicity, .data$Sex) %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$Section, .data$Line, .data$RaceEthnicity, .data$Sex) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=GR1",
                     PART = "PART=B",
                     SECTION = paste0("SECTION=", .data$Section),
                     LINE = paste0("LINE=", .data$Line),
                     RACE = paste0("RACE=", .data$RaceEthnicity),
                     SEX = paste0("SEX=", .data$Sex),
                     COUNT = paste0("COUNT=", .data$Count)
                     )

  #create the txt file
  write_report(df = partB,
               component = "GradRates",
               part = "PartB",
               output = output,
               format = format)
}
