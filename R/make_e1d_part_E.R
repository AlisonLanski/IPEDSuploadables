#' Make 12 Month Enrollment Part E
#'
#' R/E and Gender counts for dual enrollment (high school students)
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

make_e1d_part_E <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

    partE <- df %>%
      dplyr::select("UNITID",
                    "STUDENTID",
                    "ISHIGHSCHOOL",
                    "RACEETHNICITY",
                    "SEX") %>%
      dplyr::filter(.data$ISHIGHSCHOOL == 1) %>%
      dplyr::distinct() %>%
      dplyr::group_by(.data$UNITID,
                      .data$RACEETHNICITY,
                      .data$SEX) %>%
      dplyr::summarize(COUNT = n()) %>%
      dplyr::ungroup() %>%
      #sort for easy viewing
      dplyr::arrange(.data$RACEETHNICITY,
                     .data$SEX) %>%
      #format for upload
      dplyr::transmute(UNITID = .data$UNITID,
                       SURVSECT = "E1D",
                       PART = "E",
                       RACE = .data$RACEETHNICITY,
                       SEX = .data$SEX,
                       COUNT = .data$COUNT
      )


  return(partE)
}
