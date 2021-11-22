#' Some initial recodes for OutcomeMeasures
#'
#' @param df a dataframe of student level data
#'
#' @importFrom stringr str_to_upper
#' @importFrom dplyr mutate case_when
#'
#' @return df ready for om report scripts
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
    print("Some COHORTSTATUS recoding has failed. Please recheck that all rows only have allowed values")
  }

  return(df)
}
