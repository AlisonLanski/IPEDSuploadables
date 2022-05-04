#' Some initial recoding for Completions
#'
#' @param df a dataframe of student level data or cip information
#'
#' @importFrom dplyr case_when mutate select
#' @importFrom tidyr separate
#'
#' @importFrom rlang .data
#' @importFrom stringr str_to_upper
#'
#' @return df
#' @export
#'

prep_com_data_frame <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #cips could be 6-digit characters: if so, add the period
  if(sum(grepl(df$MAJORCIP, pattern = "^[0-9]{6}$")) == nrow(df)){
    df <- df %>%
      dplyr::mutate(MAJORCIP = gsub(pattern = "(^[0-9]{2})([0-9]{4}$)",
                                    replacement = "\\1\\.\\2",
                                    x = .data$MAJORCIP))
  }

  #now check that all cips have a period... if not, throw a warning
  if(sum(!grepl(df$MAJORCIP, pattern = "\\.")) > 0){
    stop("Cip Codes are not in an accepted format. Review setup requirements for Completions")
  }

  #if they do have a period, only proceed if the format isn't finished
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
      nchar(.data$Four) == 0 ~ paste0(.data$Four, "0000"),
      nchar(.data$Four) == 1 ~ paste0(.data$Four, "000"),
      nchar(.data$Four) == 2 ~ paste0(.data$Four, "00"),
      nchar(.data$Four) == 3 ~ paste0(.data$Four, "0"),
      TRUE ~ .data$Four
    ),
    MAJORCIP = paste0(.data$Two, ".", .data$Four)
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
