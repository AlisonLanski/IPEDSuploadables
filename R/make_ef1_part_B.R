#' Make Fall Enrollment Part B
#'
#' @description Student Counts by Age/gender
#'
#' @param df A dataframe of student information
#'
#' @importFrom rlang .data
#'
#' @importFrom dplyr select group_by summarise arrange transmute n mutate
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A text file
#' @export
#'

make_ef1_part_B <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partB <- df %>%
           dplyr::select(.data$UNITID,
                         .data$ISFULLTIME,
                         .data$AGE,
                         .data$STUDENTLEVEL,
                         .data$SEX) %>%
           dplyr::mutate(LINE = dplyr::case_when(
                                         .data$ISFULLTIME == 1 & .data$AGE <= 17 ~ 1,
                                         .data$ISFULLTIME == 1 & .data$AGE <= 19 ~ 2,
                                         .data$ISFULLTIME == 1 & .data$AGE <= 21 ~ 3,
                                         .data$ISFULLTIME == 1 & .data$AGE <= 24 ~ 4,
                                         .data$ISFULLTIME == 1 & .data$AGE <= 29 ~ 5,
                                         .data$ISFULLTIME == 1 & .data$AGE <= 34 ~ 6,
                                         .data$ISFULLTIME == 1 & .data$AGE <= 39 ~ 7,
                                         .data$ISFULLTIME == 1 & .data$AGE <= 49 ~ 8,
                                         .data$ISFULLTIME == 1 & .data$AGE <= 64 ~ 9,
                                         .data$ISFULLTIME == 1 & .data$AGE >= 65 ~ 10,
                                         .data$ISFULLTIME == 0 & .data$AGE <= 17 ~ 13,
                                         .data$ISFULLTIME == 0 & .data$AGE <= 19 ~ 14,
                                         .data$ISFULLTIME == 0 & .data$AGE <= 21 ~ 15,
                                         .data$ISFULLTIME == 0 & .data$AGE <= 24 ~ 16,
                                         .data$ISFULLTIME == 0 & .data$AGE <= 29 ~ 17,
                                         .data$ISFULLTIME == 0 & .data$AGE <= 34 ~ 18,
                                         .data$ISFULLTIME == 0 & .data$AGE <= 39 ~ 19,
                                         .data$ISFULLTIME == 0 & .data$AGE <= 49 ~ 20,
                                         .data$ISFULLTIME == 0 & .data$AGE <= 64 ~ 21,
                                         .data$ISFULLTIME == 0 & .data$AGE >= 65 ~ 22
                                       ),
                         STUDENTLEVEL = dplyr::recode(.data$STUDENTLEVEL,
                                                      "Undergraduate" = 1,
                                                      "Graduate" = 3)
                       ) %>%
           dplyr::group_by(.data$UNITID,
                           .data$STUDENTLEVEL,
                           .data$LINE,
                           .data$SEX) %>%
           dplyr::summarise(COUNT = n()) %>%
           #sort for easy viewing
           dplyr::arrange(.data$LINE,
                          .data$STUDENTLEVEL,
                          .data$SEX) %>%
           dplyr::ungroup() %>%
           #format for upload
           dplyr::transmute(UNITID = .data$UNITID,
                            SURVSECT = "EF1",
                            PART = "B",
                            LINE = .data$LINE,
                            SLEVEL = .data$STUDENTLEVEL,
                            SEX = .data$SEX,
                            COUNT = .data$COUNT
                           )

}
