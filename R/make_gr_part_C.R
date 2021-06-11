#' Make Graduation Rates Part C
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#' @param cohort_year A numerical value for the fall cohort
#'
#' @importFrom rlang .data
#' @importFrom dplyr select group_by summarize ungroup arrange transmute n
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'

make_gr_part_C <- function(df, output = "part", format = "both") {

  partC <- df %>%
    dplyr::select(.data$Unitid,
                  .data$StudentID,
                  .data$RaceEthnicity,
                  .data$Sex,
                  .data$Cohort,
                  .data$IsDegreeSeeking,
                  .data$IsCertSeeking,
                  .data$IsFirstTime,
                  .data$IsFullTime,
                  .data$PellGrant,
                  .data$DirectLoan,
                  .data$IsExclusion
                  ) %>%
    dplyr::mutate(Section = case_when(
                            # 1,
                            .data$IsFullTime == 1 & .data$IsFirstTime == 1 & .data$IsDegreeSeeking == 1 ~ 2,
                            .data$IsFullTime == 1 & .data$IsFirstTime == 1 & (.data$IsDegreeSeeking | .data$IsCertSeeking == 1) ~ 3
                          ),
                  Line = case_when(
                            .data$Is2YearProgram == 1 & .data$CompletedIn150 == 1 ~ 11,
                            .data$Is2YearProgram == 0 & .data$Is2YearProgram == 0 & .data$CompletedIn150 == 1 ~ 12, ## Not sure how to do this one
                            .data$Is4YearProgram == 1 & .data$CompletedIn150 == 1 ~ 18,
                            .data$Is4YearProgram == 1 & .data$CompletedIn4Years == 1 ~ 19,
                            .data$Is4YearProgram == 1 & .data$CompletedIn5Years == 1 ~ 20
                          )
                ) %>%
    #deduplicate
    dplyr::distinct() %>%
    dplyr::filter((.data$PellGrant == 1 | .data$DirectLoan == 1)) %>%
    dplyr::mutate(Line = ifelse(.data$Line == 1, 10, .data$Line)) %>%
    dplyr::mutate(Section = ifelse(.data$Line == 10, 2, .data$Section)) %>%
    dplyr::filter(.data$Section == 2, .data$Line %in% c(10, 18, 29, 45)) %>%
    #aggregate and count
    dplyr::group_by(.data$Unitid, .data$Section, .data$Line) %>%
    dplyr::summarize(TotalPell = sum(.data$PellGrant),
                     TotalLoan = sum(.data$DirectLoan)) %>%
    dplyr::ungroup() %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=GR1",
                     PART = "PART=C",
                     SECTION = paste0("SECTION=", .data$Section),
                     LINE = paste0("LINE=", .data$Line),
                     PELLGRANT_RCPT = paste0("PELLGRANT_RCPT=", .data$TotalPell),
                     DIRECTLOAN_RCPT = paste0("DIRECTLOAN_RCPT=", .data$TotalLoan)
    )


  write_report(df = partC,
               component = "GradRates",
               part = "PartC",
               output = output,
               format = format)
}
