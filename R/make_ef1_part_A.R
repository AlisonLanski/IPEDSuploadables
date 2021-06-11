#' Make Fall Enrollment Part A
#'
#' @param df A dataframe of student/degree information
#' @param extracips A dataframe of cips offered by the institution but not in \code{'df'}
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by summarize arrange transmute n mutate
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'


make_ef1_part_A <- function(df, extracips = NULL, output = "part", format = "both") {

  partA <- df %>%
    dplyr::select(.data$Unitid,
                  .data$StudentID,
                  .data$MajorCIP,
                  .data$IsFullTime,
                  .data$IsFirstTime,
                  .data$IsTransfer,
                  .data$IsDegreeCertSeeking,
                  .data$StudentLevel,
                  .data$RaceEthnicity,
                  .data$Sex) %>%
    dplyr::mutate(Line = dplyr::case_when(
                                      .data$IsFullTime == 1 & .data$IsFirstTime == 1 & .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 1,
                                      .data$IsFullTime == 1 & .data$IsTransfer == 1 & .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 2,
                                      .data$IsFullTime == 1 & .data$IsFirstTime == 0 & .data$IsTransfer == 0 & .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 3,
                                      .data$IsFullTime == 1 & .data$IsDegreeCertSeeking == 0 & .data$StudentLevel == "Undergraduate" ~ 7,
                                      .data$IsFullTime == 1 & .data$StudentLevel == "Graduate" ~ 11,
                                      .data$IsFullTime == 0 & .data$IsFirstTime == 1 & .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 15,
                                      .data$IsFullTime == 0 & .data$IsTransfer == 1 & .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 16,
                                      .data$IsFullTime == 0 & .data$IsFirstTime == 0 & .data$IsTransfer == 0 & .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 17,
                                      .data$IsFullTime == 0 & .data$IsDegreeCertSeeking == 0 ~ 21,
                                      .data$IsFullTime == 0 & .data$StudentLevel == "Graduate" ~ 25
                                    ),
                  RaceEthnicity = dplyr::recode(.data$RaceEthnicity,
                                                "NONRS" = 1,
                                                "HISPA" = 2,
                                                "AIAKN" = 3,
                                                "ASIAN" = 4,
                                                "BLACK" = 5,
                                                "PACIF" = 6,
                                                "WHITE" = 7,
                                                "MULTI" = 8,
                                                "UNKWN" = 9
                                                ),
                  Sex = dplyr::recode(.data$Sex,
                                      "M" = 1,
                                      "F" = 2)
                  ) %>%
    dplyr::group_by(.data$MajorCip, .data$Line, .data$RaceEthnicity, .data$Sex) %>%
    dplyr::summarise(Count = n()) %>%
    #sort for easy viewing
    dplyr::arrange(.data$MajorCip, .data$Line, .data$RaceEthnicity, .data$Sex) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=EF1",
                     PART = "PART=A",
                     CIPCODE = paste0("CIPCODE=", .data$MajorCip),
                     LINE = paste0("LINE=", .data$Line),
                     RACE = paste0("RACE=", .data$RaceEthnicity),
                     SEX = paste0("SEX=", .data$Sex),
                     COUNT = paste0("COUNT=", .data$Count)
    )

  #create the txt file
  write_report(df = partA,
               component = "FallEnrollment",
               part = "PartA",
               output = output,
               format = format)
}
