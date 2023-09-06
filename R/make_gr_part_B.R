#' Make Graduation Rates Part B
#'
#' @param df A dataframe of student/degree information
#'
#' @importFrom rlang .data
#'
#' @importFrom dplyr select group_by summarize ungroup bind_rows arrange transmute n
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A text file
#' @export
#'

make_gr_part_B <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #####
  #prep section 1 (revised cohort totals)
  #prep line 1 (everyone by RE/SEX)
  partB_section1_line1 <- df %>%
                          dplyr::mutate(SECTION = 1,
                                        LINE = 1) %>%
                          dplyr::select("UNITID",
                                        "RACEETHNICITY",
                                        "SEX",
                                        "SECTION",
                                        "LINE")

  #prep line 2 (only BA-seeking or equiv by RE/SEX)
  partB_section1_line2 <- df %>%
                          dplyr::filter(.data$ENTERINGPROGRAMTYPE == 3) %>%
                          dplyr::mutate(SECTION = 1,
                                        LINE = 2) %>%
                          dplyr::select("UNITID",
                                        "RACEETHNICITY",
                                        "SEX",
                                        "SECTION",
                                        "LINE")

  #####
  #prep section 2
  #lines 11-18 (all completers by current program type/pct + RE/SEX)
  partB_section23_toline18 <- df %>%
                              dplyr::mutate(SECTION = case_when(
                                                         .data$ENTERINGPROGRAMTYPE == 3 ~ 2,
                                                         TRUE ~ 3
                                                      ),
                                            LINE = case_when(
                                                        .data$CURRENTPROGRAMTYPE == 1 & .data$COMPLETED150 == 1 ~ 11,
                                                        .data$CURRENTPROGRAMTYPE == 2 & .data$COMPLETED150 == 1 ~ 12,
                                                        .data$CURRENTPROGRAMTYPE == 3 & .data$COMPLETED150 == 1 ~ 18
                                                      )
                                            ) %>%
                              dplyr::select("UNITID",
                                            "RACEETHNICITY",
                                            "SEX",
                                            "SECTION",
                                            "LINE")

  ###
  #prep section 2 (BA folks)
  #lines 19-51 (BA completers by yrs/non-completers by type + RE/SEX)
  partB_section2_toline51 <- df %>%
                             dplyr::filter(.data$ENTERINGPROGRAMTYPE == 3) %>%
                             dplyr::mutate(SECTION = 2,
                                           LINE = case_when(
                                                     .data$CURRENTPROGRAMTYPE == 3 &
                                                       .data$COMPLETED150 == 1 &
                                                       .data$COMPLETEDFOURYEARS == 1 ~ 19,

                                                     .data$CURRENTPROGRAMTYPE == 3 &
                                                       .data$COMPLETED150 == 1 &
                                                       .data$COMPLETEDFIVEYEARS == 1 ~ 20,

                                                     .data$ISTRANSFEROUT == 1 ~ 30,

                                                     .data$ISEXCLUSION == 1 ~ 45,

                                                     .data$ISSTILLENROLLED == 1 ~ 51
                                                     )
                             ) %>%
                             dplyr::select("UNITID",
                                           "RACEETHNICITY",
                                           "SEX",
                                           "SECTION",
                                           "LINE")

  ###
  #prep section 3 (non-BA folks)
  #lines 30-51 (non-completers by RE/SEX)
  partB_section3_toline51 <- df %>%
                             dplyr::filter(.data$ENTERINGPROGRAMTYPE < 3 ) %>%
                             dplyr::mutate(SECTION = 3,
                                           LINE = case_when(
                                             .data$ISTRANSFEROUT == 1 ~ 30,
                                             .data$ISEXCLUSION == 1 ~ 45,
                                             .data$ISSTILLENROLLED == 1 ~ 51
                                           )
                             ) %>%
                             dplyr::select("UNITID",
                                           "RACEETHNICITY",
                                           "SEX",
                                           "SECTION",
                                           "LINE")


  #put it all together and count things up
  partB <- dplyr::bind_rows(partB_section1_line1,
                            partB_section1_line2,
                            partB_section23_toline18,
                            partB_section2_toline51,
                            partB_section3_toline51) %>%
          #remove extraneous rows
          dplyr::filter(!is.na(.data$LINE)) %>%
          #aggregate
          dplyr::group_by(.data$UNITID,
                          .data$SECTION,
                          .data$LINE,
                          .data$RACEETHNICITY,
                          .data$SEX) %>%
          dplyr::summarize(COUNT = dplyr::n()) %>%
          dplyr::ungroup() %>%
          #sort for easy viewing
          dplyr::arrange(.data$SECTION,
                         .data$LINE,
                         .data$RACEETHNICITY,
                         .data$SEX) %>%
          #format for upload
          dplyr::transmute(UNITID = .data$UNITID,
                           SURVSECT = "GR1",
                           PART = "B",
                           SECTION = .data$SECTION,
                           LINE = .data$LINE,
                           RACE = .data$RACEETHNICITY,
                           SEX = .data$SEX,
                           COUNT = .data$COUNT
          )

}
