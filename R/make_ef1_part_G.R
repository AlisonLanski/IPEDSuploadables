#' Make Fall Enrollment Part G
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by summarize arrange transmute n
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'


make_ef1_part_G <- function(df, output = "part", format = "both") {

  partG <- df %>%
    dplyr::select(.data$Unitid,
                  .data$StudentID,
                  .data$IsFullTime,
                  .data$IsFirstTime,
                  .data$IsTransfer,
                  .data$IsDegreeCertSeeking,
                  .data$Age,
                  .data$StudentLevel,
                  .data$RaceEthnicity,
                  .data$Sex) %>%
    dplyr::mutate(Line = dplyr::case_when(
                            .data$IsFullTime == 1 & .data$IsFirstTime == 1 & .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 1,
                            .data$IsFullTime == 1 & .data$IsTransfer == 1 & .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 2,
                            .data$IsFullTime == 1 & .data$IsFirstTime == 0 & .data$IsTransfer == 0 & .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 3
                          )
                  ) %>%
    #sort for easy viewing
    dplyr::arrange(.data$Line) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=EF1",
                     PART = "PART=G",
                     LINE = paste0("CIPCODE=", .data$Line),
                     ENROLL_EXCLUSIVE = paste0("AWLEVEL=", .data$EnrollExclusive),
                     ENROLL_SOME = paste0("RACE=", .data$EnrollSome),
                     NOTENROLL = paste0("SEX=", .data$NotEnroll),
                     INUS_PPS = paste0("INUS_PPS=", .data$PPS),
                     INUS_NOTPPS = paste0("INUS_NOTPPS=", .data$NotPPS),
                     INUS_UNKNOWN_STATE = paste0("INUS_UNKNOWN_STATE=", .data$UnknownState),
                     OUTSIDEUS = paste0("OUTSIDEUS=", .data$OutsideUS)
    )

  #create the txt file
  write_report(df = partG,
               component = "FallEnrollment",
               part = "PartG",
               output = output,
               format = format)
}
