#' Make Fall Enrollment Part G
#'
#' @description Distance Ed component of part
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by summarize arrange transmute n
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A text file
#' @export
#'

make_ef1_part_G <- function(df, output = "part", format = "both") {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partG <- df %>%
           dplyr::select(.data$UNITID,
                         .data$UNITIDSTATE,
                         .data$STUDENTID,
                         .data$ISDEGREECERTSEEKING,
                         .data$STUDENTLEVEL,
                         .data$ONLINESTATE,
                         .data$DISTANCEED) %>%
           dplyr::mutate(INUS_INSTATE = case_when(.data$ONLINESTATE == .data$UNITIDSTATE &
                                                    .data$DISTANCEED == 2 ~  1,
                                                  TRUE ~ 0),
                         INUS_OUTSTATE = case_when(.data$ONLINESTATE != .data$UNITIDSTATE &
                                                     .data$ONLINESTATE <= 78 & .data$UNITIDSTATE != 57 &
                                                     .data$DISTANCEED == 2 ~  1,
                                                   TRUE ~ 0),
                         INUS_UNKNOWN = case_when(.data$ONLINESTATE == 57 &
                                                    .data$DISTANCEED == 2 ~  1,
                                                  TRUE ~ 0),
                         OUTUS = case_when(.data$ONLINESTATE == 90 &
                                             .data$DISTANCEED == 2 ~  1,
                                           TRUE ~ 0)
                         ) %>%
           dplyr::mutate(LINE = dplyr::case_when(
                                   .data$ISDEGREECERTSEEKING == 1 & .data$STUDENTLEVEL == "Undergraduate" ~ 1,
                                   .data$ISDEGREECERTSEEKING == 0 & .data$STUDENTLEVEL == "Undergraduate" ~ 2,
                                   .data$STUDENTLEVEL == "Graduate" ~ 3
                                 )
                         ) %>%
           dplyr::mutate(ENROLL_EXC = ifelse(.data$DISTANCEED == 2, 1, 0),
                         ENROLL_SOME = ifelse(.data$DISTANCEED == 1, 1, 0)) %>%
           dplyr::group_by(.data$UNITID,
                           .data$LINE) %>%
           dplyr::summarize(ENROLLEXCLUSIVE = sum(.data$ENROLL_EXC, na.rm = T),
                            ENROLLSOME = sum(.data$ENROLL_SOME, na.rm = T),
                            PPS = sum(.data$INUS_INSTATE, na.rm = T),
                            NOTPPS = sum(.data$INUS_OUTSTATE, na.rm = T),
                            UNKNOWNSTATE = sum(.data$INUS_UNKNOWN, na.rm = T),
                            OUTSIDEUS = sum(.data$OUTUS, na.rm = T)) %>%
           dplyr::ungroup() %>%
           #sort for easy viewing
           dplyr::arrange(.data$LINE) %>%
           #format for upload
           dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                            SURVSECT = "SURVSECT=EF1",
                            PART = "PART=G",
                            LINE = paste0("LINE=", .data$LINE),
                            ENROLL_EXCLUSIVE = paste0("ENROLL_EXCLUSIVE=", .data$ENROLLEXCLUSIVE),
                            ENROLL_SOME = paste0("ENROLL_SOME=", .data$ENROLLSOME),
                            INUS_PPS = paste0("INUS_PPS=", .data$PPS),
                            INUS_NOTPPS = paste0("INUS_NOTPPS=", .data$NOTPPS),
                            INUS_UNKNOWN_STATE = paste0("INUS_UNKNOWN_STATE=", .data$UNKNOWNSTATE),
                            OUTSIDEUS = paste0("OUTSIDEUS=", .data$OUTSIDEUS)
                           )

  #create the txt file
  write_report(df = partG,
               component = "FallEnrollment",
               part = "PartG",
               output = output,
               format = format)
}
