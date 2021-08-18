#' Make Fall Enrollment Part B
#'
#' @description Student counts by age/gender
#'
#' @param df A dataframe of student information
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


make_ef1_part_B <- function(df, output = "part", format = "both") {

  partB <- df %>%
    dplyr::select(.data$Unitid,
                  .data$IsFullTime,
                  .data$Age,
                  .data$StudentLevel,
                  .data$RaceEthnicity,
                  .data$Sex) %>%
    dplyr::mutate(Line = dplyr::case_when(
                                  .data$IsFullTime == 1 & .data$Age <= 17 ~ 1,
                                  .data$IsFullTime == 1 & .data$Age <= 19 ~ 2,
                                  .data$IsFullTime == 1 & .data$Age <= 21 ~ 3,
                                  .data$IsFullTime == 1 & .data$Age <= 24 ~ 4,
                                  .data$IsFullTime == 1 & .data$Age <= 29 ~ 5,
                                  .data$IsFullTime == 1 & .data$Age <= 34 ~ 6,
                                  .data$IsFullTime == 1 & .data$Age <= 39 ~ 7,
                                  .data$IsFullTime == 1 & .data$Age <= 49 ~ 8,
                                  .data$IsFullTime == 1 & .data$Age <= 64 ~ 9,
                                  .data$IsFullTime == 1 & .data$Age >= 65 ~ 10,
                                  .data$IsFullTime == 0 & .data$Age <= 17 ~ 13,
                                  .data$IsFullTime == 0 & .data$Age <= 19 ~ 14,
                                  .data$IsFullTime == 0 & .data$Age <= 21 ~ 15,
                                  .data$IsFullTime == 0 & .data$Age <= 24 ~ 16,
                                  .data$IsFullTime == 0 & .data$Age <= 29 ~ 17,
                                  .data$IsFullTime == 0 & .data$Age <= 34 ~ 18,
                                  .data$IsFullTime == 0 & .data$Age <= 39 ~ 19,
                                  .data$IsFullTime == 0 & .data$Age <= 49 ~ 20,
                                  .data$IsFullTime == 0 & .data$Age <= 64 ~ 21,
                                  .data$IsFullTime == 0 & .data$Age >= 65 ~ 22
                                ),
                  StudentLevel = dplyr::recode(.data$StudentLevel,
                                               "Undergraduate" = 1,
                                               "Graduate" = 3)
                ) %>%
    dplyr::group_by(.data$Unitid, .data$Line, .data$RaceEthnicity, .data$Sex, .data$StudentLevel) %>%
    dplyr::summarise(Count = n()) %>%
    #sort for easy viewing
    dplyr::arrange(.data$Line, .data$StudentLevel, .data$Sex) %>%
    dplyr::ungroup() %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=EF1",
                     PART = "PART=B",
                     LINE = paste0("LINE=", .data$Line),
                     SLEVEL = paste0("RACE=", .data$StudentLevel),
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
