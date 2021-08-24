#' Make Graduation Rates Part B
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by summarize ungroup bind_rows arrange transmute n
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'


make_gr_part_B <- function(df, output = "part", format = "both") {

  #####
  #prep section 1 (revised cohort totals)
  #prep line 1 (everyone by RE/Sex)
  partB_section1_line1 <- df %>%
    dplyr::mutate(Section = 1,
                  Line = 1) %>%
    dplyr::select(.data$Unitid,
                  .data$RaceEthnicity,
                  .data$Sex,
                  .data$Section,
                  .data$Line)

  #prep line 2 (only BA-seeking or equiv by RE/Sex)
  partB_section1_line2 <- df %>%
    dplyr::filter(.data$ProgramType == 3) %>%
    dplyr::mutate(Section = 1,
                  Line = 2) %>%
    dplyr::select(.data$Unitid,
                  .data$RaceEthnicity,
                  .data$Sex,
                  .data$Section,
                  .data$Line)

  #####
  #prep sections 2 and 3
  #lines 11-18 (completers by program type/pct + RE/Sex)
  partB_section23_toline18 <- df %>%
    dplyr::mutate(Section = case_when(
                               .data$ProgramType == 3 ~ 2,
                               TRUE ~ 3
                            ),
                  Line = case_when(
                              .data$ProgramType == 1 & .data$Completed150 == 1 ~ 11,
                              .data$ProgramType == 2 & .data$Completed150 == 1 ~ 12,
                              .data$ProgramType == 3 & .data$Completed150 == 1 ~ 18
                            )
                  ) %>%
    dplyr::select(.data$Unitid,
                  .data$RaceEthnicity,
                  .data$Sex,
                  .data$Section,
                  .data$Line)

  ###
  #prep section 2 (BA folks)
  #lines 19-51 (BA completers by yrs/non-completers by type + RE/Sex)
  partB_section2_toline51 <- df %>%
    dplyr::filter(.data$ProgramType == 3) %>%
    dplyr::mutate(Section = 2,
                  Line = case_when(
                            .data$CompletedFourYears == 1 ~ 19,
                            .data$CompletedFiveYears == 1 ~ 20,
                            .data$IsTransferOut == 1 ~ 30,
                            .data$IsExclusion == 1 ~ 45,
                            .data$IsStillEnrolled == 1 ~ 51
                            )
    ) %>%
    dplyr::select(.data$Unitid,
                  .data$RaceEthnicity,
                  .data$Sex,
                  .data$Section,
                  .data$Line)


  ###
  #prep section 3 (non-BA folks)
  #lines 30-51 (non-completers by RE/Sex)
  partB_section3_toline51 <- df %>%
    dplyr::filter(.data$ProgramType < 3 ) %>%
    dplyr::mutate(Section = 3,
                  Line = case_when(
                    .data$IsTransferOut == 1 ~ 30,
                    .data$IsExclusion == 1 ~ 45,
                    .data$IsStillEnrolled == 1 ~ 51
                  )
    ) %>%
    dplyr::select(.data$Unitid,
                  .data$RaceEthnicity,
                  .data$Sex,
                  .data$Section,
                  .data$Line)


  #put it all together and count things up
  partB <- bind_rows(partB_section1_line1,
                     partB_section1_line2,
                     partB_section23_toline18,
                     partB_section2_toline51,
                     partB_section3_toline51) %>%
    #remove extraneous rows
    dplyr::filter(!is.na(.data$Line)) %>%
    #aggregate
    dplyr::group_by(.data$Unitid,
                    .data$Section,
                    .data$Line,
                    .data$RaceEthnicity,
                    .data$Sex) %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$Section,
                   .data$Line,
                   .data$RaceEthnicity,
                   .data$Sex) %>%
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
