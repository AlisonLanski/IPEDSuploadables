#' Some initial recoding for OutcomeMeasures
#'
#' @param df a dataframe of student level data
#'
#' @importFrom stringr str_to_upper
#' @importFrom dplyr mutate case_when
#'
#' @return A dataframe ready for the make_om scripts
#' @export
#'

prep_om_data_frame <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  df <- df %>%
        dplyr::mutate(UNITID = as.character(.data$UNITID),
                      EXCLUSION = dplyr::case_when(
                                              .data$COHORTSTATUS == "Include" ~ FALSE,
                                              .data$COHORTSTATUS == "Exclude" ~ TRUE,
                                              TRUE ~ NA
                                            )
                      )

  if (sum(is.na(df$EXCLUSION)) > 0) {
    warning(paste0("Check CohortStatus: invalid values found for StudentId: ", toString(df$STUDENTID[is.na(df$EXCLUSION)])))
  }

  return(df)
}
