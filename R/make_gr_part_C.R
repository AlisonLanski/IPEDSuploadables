#' Make Graduation Rates Part C
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom dplyr select group_by summarize ungroup arrange transmute n
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'

make_gr_part_C <- function(df, output = "part", format = "both") {

  #everyone
  partC_toline10 <- df %>%
    dplyr::mutate(Section = case_when(
                              .data$ProgramType == 3 ~ 2,
                              TRUE ~ 3
                              ),
                  Line = 10) %>%
    dplyr::select(.data$Unitid,
                  .data$PellGrant,
                  .data$DirectLoan,
                  .data$Section,
                  .data$Line)

  #BA completers by 150
  partC_toline18 <- df %>%
    dplyr::filter(.data$ProgramType == 3,
                  .data$Completed150 == 1) %>%
    dplyr::mutate(Section = 2,
                  Line = 18) %>%
    dplyr::select(.data$Unitid,
                  .data$PellGrant,
                  .data$DirectLoan,
                  .data$Section,
                  .data$Line)

  #everyone by 150 and exclusions
  partC_toline45 <- df %>%
    dplyr::mutate(Section = case_when(
                                .data$ProgramType == 3 ~ 2,
                                TRUE ~ 3),
                  Line = case_when(.data$Completed150 == 1 ~ 29,
                                   .data$IsExclusion == 1 ~ 45)) %>%
    dplyr::select(.data$Unitid,
                  .data$PellGrant,
                  .data$DirectLoan,
                  .data$Section,
                  .data$Line)

  #put it together
  partC <- dplyr::bind_rows(partC_toline10,
                            partC_toline18,
                            partC_toline45) %>%

    #remove extraneous rows
    dplyr::filter((.data$PellGrant == 1 | .data$DirectLoan == 1)) %>%
    dplyr::filter(!is.na(.data$Line)) %>%

    #clean up Pell vs DirectLoan
    dplyr::mutate(DirectLoanOnly = case_when(.data$PellGrant == 1 ~ 0,
                                                   .data$DirectLoan == 1 ~ 1,
                                                   TRUE ~ 0)) %>%
    #aggregate and count
    dplyr::group_by(.data$Unitid,
                    .data$Section,
                    .data$Line) %>%
    dplyr::summarize(TotalPell = sum(.data$PellGrant, na.rm = TRUE),
                     TotalLoan = sum(.data$DirectLoanOnly, na.rm = TRUE)) %>%
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
