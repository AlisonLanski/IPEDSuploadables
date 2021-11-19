#' Some initial recodes for Completions
#'
#' @param df a dataframe of student level data or cip information
#'
#' @importFrom dplyr case_when mutate select
#' @importFrom tidyr separate
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @importFrom stringr str_to_upper
#'
#' @return df
#' @export
#'

prep_com_data_frame <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  df <- df %>%
    tidyr::separate(col = .data$MAJORCIP,
             into = c("Two", "Four"),
             sep = "\\."
    ) %>%
    dplyr::mutate(Two = dplyr::case_when(
      nchar(.data$Two) == 1 ~ paste0("0", .data$Two),
      TRUE ~ .data$Two
    ),
    Four = dplyr::case_when(
      nchar(.data$Four) == 1 ~ paste0(.data$Four, "000"),
      nchar(.data$Four) == 2 ~ paste0(.data$Four, "00"),
      nchar(.data$Four) == 3 ~ paste0(.data$Four, "0"),
      TRUE ~ .data$Four
    ),
    MAJORCIP = paste0(.data$Two, '.', .data$Four)
    ) %>%
    dplyr::select(-.data$Two, -.data$Four) %>%
    dplyr::mutate(UNITID = as.character(.data$UNITID),
                  DEGREELEVEL = as.character(.data$DEGREELEVEL))

  if("STUDENTID" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(STUDENTID = as.character(.data$STUDENTID))
  }

  return(df)
}
