#' Some initial recodes for Completions
#'
#' @param df a dataframe of student level data
#'
#' @return df
#' @importFrom dplyr case_when mutate select
#' @importFrom tidyr separate
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#'
#' @examples
prep_com_data_files <- function(df) {

  df <- df %>%
    separate(col = MajorCip,
             into = c("Two", "Four"),
             sep = "\\."
    ) %>%
    mutate(Two = case_when(
      nchar(Two) == 1 ~ paste0("0", Two),
      TRUE ~ Two
    ),
    Four = case_when(
      nchar(Four) == 1 ~ paste0(Four, "000"),
      nchar(Four) == 2 ~ paste0(Four, "00"),
      nchar(Four) == 3 ~ paste0(Four, "0"),
      TRUE ~ Four
    ),
    MajorCip = paste0(Two, '.', Four)
    ) %>%
    select(-Two, -Four) %>%
    mutate(Unitid = as.character(Unitid))

  if('StudentId' %in% colnames(df)) {
    df <- df %>%
      mutate(StudentId = as.character(StudentId))
  }

  return(df)
}
