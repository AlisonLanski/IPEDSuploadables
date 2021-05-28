#' Make 12 Month Enrollment Part A
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


make_e1d_part_A <- function(df, output = "part", format = "both") {

  partA <- df %>%
    dplyr::select(.data$IsFullTime,
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
                                  .data$IsFullTime == 1 & .data$IsDegreeCertSeeking == 0 ~ 7,
                                  .data$IsFullTime == 0 & .data$IsFirstTime == 1 & .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 15,
                                  .data$IsFullTime == 0 & .data$IsTransfer == 1 & .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 16,
                                  .data$IsFullTime == 0 & .data$IsFirstTime == 0 & .data$IsTransfer == 0 & .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 17,
                                  .data$IsFullTime == 0 & .data$IsDegreeCertSeeking == 0 ~ 21,
                                  .data$StudentLevel == "Graduate" ~ 99
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
    dplyr::group_by(.data$Line, .data$RaceEthnicity, .data$Sex) %>%
    dplyr::summarise(Count = n()) %>%
    #sort for easy viewing
    dplyr::arrange(.data$Line, .data$RaceEthnicity, .data$Sex) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=E1D",
                     PART = "PART=A",
                     LINE = paste0("LINE=", .data$Line),
                     RACE = paste0("RACE=", .data$RaceEthnicity),
                     SEX = paste0("SEX=", .data$Sex),
                     COUNT = paste0("COUNT=", .data$Count)
                    )

  #create the txt file
  write_report(df = partA,
               component = "12MonthEnrollment",
               part = "PartA",
               output = output,
               format = format)
}
