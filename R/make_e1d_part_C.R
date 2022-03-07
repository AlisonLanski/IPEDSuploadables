#' Make 12 Month Enrollment Part C
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

make_e1d_part_C <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partC <- df %>%
    dplyr::select(.data$UNITID,
                  .data$STUDENTID,
                  .data$ISDEGREECERTSEEKING,
                  .data$STUDENTLEVEL,
                  .data$DISTANCEEDALL,
                  .data$DISTANCEEDSOME) %>%
    dplyr::mutate(LINE = case_when(
                                  .data$ISDEGREECERTSEEKING == 1 & .data$STUDENTLEVEL == "Undergraduate" ~ 1,
                                  .data$ISDEGREECERTSEEKING == 0 & .data$STUDENTLEVEL == "Undergraduate" ~ 2,
                                  .data$STUDENTLEVEL == "Graduate" ~ 3
                                )
                  ) %>%
    dplyr::select(-c(.data$ISDEGREECERTSEEKING,
                     .data$STUDENTLEVEL)) %>%
    dplyr::group_by(.data$UNITID,
                    .data$LINE) %>%
    dplyr::summarise(CountDISTANCEEDALL = sum(as.numeric(.data$DISTANCEEDALL)),
                     CountDISTANCEEDSOME = sum(as.numeric(.data$DISTANCEEDSOME))
                     ) %>%
    dplyr::ungroup() %>%
    #format for upload
    dplyr::transmute(UNITID = .data$UNITID,
                     SURVSECT = "E1D",
                     PART = "C",
                     LINE = .data$LINE,
                     ENROLL_EXCLUSIVE = .data$CountDISTANCEEDALL,
                     ENROLL_SOME = .data$CountDISTANCEEDSOME
                    )
}
