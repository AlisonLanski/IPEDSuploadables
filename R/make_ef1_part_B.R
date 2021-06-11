#' Make Fall Enrollment Part B
#'
#' @param df A dataframe of student/degree information
#' @param extracips A dataframe of cips offered by the institution but not in \code{'df'}
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by summarise arrange transmute n mutate
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'


make_ef1_part_B <- function(df, extracips = NULL, output = "part", format = "both") {

  partB <- df %>%
    dplyr::select(.data$Unitid,
                  .data$StudentID,
                  .data$MajorCIP,
                  .data$IsFullTime,
                  .data$Age,
                  .data$StudentLevel,
                  .data$RaceEthnicity,
                  .data$Sex) %>%
    dplyr::mutate(Line = dplyr::case_when(
                                  .data$IsFullTime == 1 & .data$Age < 18 ~ 1,
                                  .data$IsFullTime == 1 & .data$Age %in% c(18, 19) ~ 2,
                                  .data$IsFullTime == 1 & .data$Age %in% c(20, 21) ~ 3,
                                  .data$IsFullTime == 1 & .data$Age %in% c(22, 23, 24) ~ 4,
                                  .data$IsFullTime == 1 & .data$Age %in% c(25, 26, 27, 28, 29) ~ 5,
                                  .data$IsFullTime == 1 & .data$Age %in% c(30, 31, 32, 33, 34) ~ 6,
                                  .data$IsFullTime == 1 & .data$Age %in% c(35, 36, 37, 38, 39) ~ 7,
                                  .data$IsFullTime == 1 & .data$Age %in% c(40, 41, 42, 43, 44,
                                                                           45, 46, 47, 48, 49) ~ 8,
                                  .data$IsFullTime == 1 & .data$Age %in% c(50, 51, 52, 53, 54,
                                                                           55, 56, 57, 58, 59,
                                                                           60, 61, 62, 63, 64) ~ 9,
                                  .data$IsFullTime == 1 & .data$Age > 65 ~ 10,
                                  .data$IsFullTime == 0 & .data$Age < 18 ~ 13,
                                  .data$IsFullTime == 0 & .data$Age %in% c(18, 19) ~ 14,
                                  .data$IsFullTime == 0 & .data$Age %in% c(20, 21) ~ 15,
                                  .data$IsFullTime == 0 & .data$Age %in% c(22, 23, 24) ~ 16,
                                  .data$IsFullTime == 0 & .data$Age %in% c(25, 26, 27, 28, 29) ~ 17,
                                  .data$IsFullTime == 0 & .data$Age %in% c(30, 31, 32, 33, 34) ~ 18,
                                  .data$IsFullTime == 0 & .data$Age %in% c(35, 36, 37, 38, 39) ~ 19,
                                  .data$IsFullTime == 0 & .data$Age %in% c(40, 41, 42, 43, 44,
                                                                           45, 46, 47, 48, 49) ~ 20,
                                  .data$IsFullTime == 0 & .data$Age %in% c(50, 51, 52, 53, 54,
                                                                           55, 56, 57, 58, 59,
                                                                           60, 61, 62, 63, 64) ~ 21,
                                  .data$IsFullTime == 0 & .data$Age > 65 ~ 22
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
                                      "F" = 2),
                  StudentLevel = dplyr::recode(.data$StudentLevel,
                                               "Undergraduate" = 1,
                                               "Graduate" = 3)
                ) %>%
    dplyr::group_by(.data$MajorCip, .data$Line, .data$RaceEthnicity, .data$Sex, .data$StudentLevel) %>%
    dplyr::summarise(Count = n()) %>%
    #sort for easy viewing
    dplyr::arrange(.data$Line, .data$Level, .data$Sex) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=EF1",
                     PART = "PART=B",
                     LINE = paste0("LINE=", .data$Line),
                     SLEVEL = paste0("RACE=", .data$Level),
                     SEX = paste0("SEX=", .data$Sex),
                     COUNT = paste0("COUNT=", .data$Count)
                    )

  #create the txt file
  write_report(df = partB,
               component = "FallEnrollment",
               part = "PartB",
               output = output,
               format = format)
}
