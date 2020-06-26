#' Some initial recodes for Completions
#'
#' @param df a dataframe of student level data or cip information
#'
#' @return df
#' @importFrom dplyr case_when mutate select
#' @importFrom tidyr separate
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#'
prep_com_data_frame <- function(df) {

  df <- df %>%
    tidyr::separate(col = .data$MajorCip,
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
    MajorCip = paste0(.data$Two, '.', .data$Four)
    ) %>%
    dplyr::select(-.data$Two, -.data$Four) %>%
    dplyr::mutate(Unitid = as.character(.data$Unitid))

  if('StudentId' %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(StudentId = as.character(.data$StudentId))
  }

  return(df)
}
