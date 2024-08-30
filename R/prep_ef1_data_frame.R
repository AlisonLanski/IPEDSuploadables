#' Some initial recoding for Fall Enrollment
#'
#' @param df a dataframe of student level data
#'
#' @importFrom dplyr case_when mutate
#' @importFrom tidyr separate
#'
#' @importFrom rlang .data
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe ready for the make_ef1 scripts
#' @export
#'

prep_ef1_data_frame <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  df <- df %>%
    dplyr::mutate(UNITID = as.character(.data$UNITID))

  if("STUDENTID" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(STUDENTID = as.character(.data$STUDENTID))
  }

  return(df)
}


