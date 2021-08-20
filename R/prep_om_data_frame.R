#' Some initial recodes for OutcomeMeasures
#'
#' @param df a dataframe of student level data
#'
#' @return df ready for om report scripts
#'
#' @export
#'
prep_om_data_frame <- function(df) {

  df <- df %>%
        dplyr::mutate(Unitid = as.character(.data$Unitid),
                      Exclusion = dplyr::case_when(
                        .data$CohortStatus == 'Include' ~ FALSE,
                        .data$CohortStatus == 'Exclude' ~ TRUE,
                        TRUE ~ NA))

  if(sum(is.na(df$Exclusion)) > 0) {
    print("Some CohortStatus recoding has failed. Please recheck that all rows only have allowed values")
  }

  return(df)
}
