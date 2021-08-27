#' Make Fall Enrollment Part G
#' @description Distance Ed component of part A
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
                  .data$UnitidState,
                  .data$StudentId,
                  .data$IsDegreeCertSeeking,
                  .data$StudentLevel,
                  .data$State,
                  .data$DistanceEd) %>%
    dplyr::mutate(InUS_InState = case_when(.data$State == .data$UnitidState &
                                             .data$DistanceEd == 2 ~  1,
                                           TRUE ~ 0),
                  InUS_OutState = case_when(.data$State != .data$UnitidState &
                                              .data$State <= 78 & .data$UnitidState != 57 &
                                              .data$DistanceEd == 2 ~  1,
                                            TRUE ~ 0),
                  InUS_Unknown = case_when(.data$State == 57 &
                                             .data$DistanceEd == 2 ~  1,
                                           TRUE ~ 0),
                  OutUS = case_when(.data$State == 90 &
                                      .data$DistanceEd == 2 ~  1,
                                    TRUE ~ 0)
                  ) %>%
    dplyr::mutate(Line = dplyr::case_when(
                            .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 1,
                            .data$IsDegreeCertSeeking == 0 & .data$StudentLevel == "Undergraduate" ~ 2,
                            .data$StudentLevel == "Graduate" ~ 3
                          )
                  ) %>%
    dplyr::mutate(Enroll_Exc = ifelse(.data$DistanceEd == 2, 1, 0),
                  Enroll_Some = ifelse(.data$DistanceEd == 1, 1, 0)) %>%
    dplyr::group_by(.data$Unitid, .data$Line) %>%
    summarize(EnrollExclusive = sum(.data$Enroll_Exc, na.rm = T),
              EnrollSome = sum(.data$Enroll_Some, na.rm = T),
              PPS = sum(.data$InUS_InState, na.rm = T),
              NotPPS = sum(.data$InUS_OutState, na.rm = T),
              UnknownState = sum(.data$InUS_Unknown, na.rm = T),
              OutsideUS = sum(.data$OutUS, na.rm = T)) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$Line) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=EF1",
                     PART = "PART=G",
                     LINE = paste0("LINE=", .data$Line),
                     ENROLL_EXCLUSIVE = paste0("ENROLL_EXCLUSIVE=", .data$EnrollExclusive),
                     ENROLL_SOME = paste0("ENROLL_SOME=", .data$EnrollSome),
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
