#' Some initial recodes for Admissions parts C and D
#'
#' @param df a dataframe of applicant level data
#'
#' @importFrom
#'

prep_adm_data_frame <- function(df) {
  colnames(df) <- stringr::str_to_upper(colnames(df))

  df <- df %>%
    dplyr::mutate(UNITID = as.character(.data$UNITID))

  if("STUDENTID" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(STUDENTID = as.character(.data$STUDENTID))
  }

  return(df)
}
