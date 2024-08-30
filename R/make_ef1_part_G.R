#' Make Fall Enrollment Part G
#'
#' @description Distance Ed counts
#'
#' @param df A dataframe of student/degree information
#'
#' @importFrom rlang .data
#'
#' @importFrom dplyr select group_by summarize arrange transmute n
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_ef1_part_G <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #need to have all options pre-pivot
  partG_dummy <- tidyr::expand_grid(UNITID = get_ipeds_unitid(df),
                                    UNITIDSTATE = df$UNITIDSTATE[1],
                                    STUDENTID = "thisisastudentid",
                                    ISDEGREECERTSEEKING = c(0, 1),
                                    STUDENTLEVEL = c("Undergraduate", "Graduate"),
                                    ONLINESTATE = c(df$UNITIDSTATE[1], 6, 57, 78, 90), #cover all the options
                                    DISTANCEED = c(1,2),
                                    counter = 0,
                                    dummyrow = 1) %>%
                 dplyr::mutate(STUDENTID = paste0(.data$STUDENTID, 1:dplyr::n()))

  partG <- df %>%
           dplyr::select("UNITID",
                         "UNITIDSTATE",
                         "STUDENTID",
                         "ISDEGREECERTSEEKING",
                         "STUDENTLEVEL",
                         "ONLINESTATE",
                         "DISTANCEED") %>%
            dplyr::mutate(counter = 1,
                          dummyrow = 0) %>%
            dplyr::bind_rows(partG_dummy) %>%
            dplyr::mutate(StateGroup = dplyr::case_when(.data$DISTANCEED != 2 ~ 'Nope',
                                         .data$ONLINESTATE == .data$UNITIDSTATE ~ 'INUS_INSTATE',
                                         .data$ONLINESTATE == 57 ~ 'INUS_UNKNOWN',
                                         .data$ONLINESTATE <= 78 ~ 'INUS_OUTSTATE',
                                         .data$ONLINESTATE == 90 ~ 'OUTUS',
                                         TRUE ~ 'Nope')) %>%
            tidyr::pivot_wider(names_from = "StateGroup",
                               values_from = "counter", values_fill = 0) %>%
            dplyr::select(-"Nope") %>%
           dplyr::mutate(LINE = dplyr::case_when(.data$ISDEGREECERTSEEKING == 1 & .data$STUDENTLEVEL == "Undergraduate" ~ 1,
                                   .data$ISDEGREECERTSEEKING == 0 & .data$STUDENTLEVEL == "Undergraduate" ~ 2,
                                   .data$STUDENTLEVEL == "Graduate" ~ 3
                                 )
                         ) %>%
           dplyr::mutate(ENROLL_EXC = ifelse(.data$DISTANCEED == 2 & .data$dummyrow == 0, 1, 0),
                         ENROLL_SOME = ifelse(.data$DISTANCEED == 1 & .data$dummyrow == 0, 1, 0)) %>%
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
           dplyr::transmute(UNITID = .data$UNITID,
                            SURVSECT = "EF1",
                            PART = "G",
                            LINE = .data$LINE,
                            ENROLL_EXCLUSIVE = .data$ENROLLEXCLUSIVE,
                            ENROLL_SOME = .data$ENROLLSOME,
                            INUS_PPS = .data$PPS,
                            INUS_NOTPPS = .data$NOTPPS,
                            INUS_UNKNOWN_STATE = .data$UNKNOWNSTATE,
                            OUTSIDEUS = .data$OUTSIDEUS
                           )

}
