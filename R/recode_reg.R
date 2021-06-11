#' Recode race/ethnicity and gender
#'
#' @param df A dataframe of student information
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate
#'
#' @return A text file
#' @export
#'


recode_reg <- function(df) {

  df <- df %>%
        dplyr::mutate(RaceEthnicity = dplyr::recode(.data$RaceEthnicity,
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
                    )

  return(df)
}
