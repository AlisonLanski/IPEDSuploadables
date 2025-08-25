#' Make Graduation Rates Part E (gender details)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#'  This section was removed by IPEDS in the 2024-2025 reporting cycle, after
#'  being required for the 2023-2024 reporting cycle. It remains here in case it
#'  is re-added in a future year. I have updated testing/produce code so nothing
#'  else calls it. By making it internal, it won't be listed but remains
#'  available.
#'
#'
#' @param df A dataframe of student/degree information for unduplicated
#'   undergraduates
#' @param ugender A boolean: TRUE means you are collecting and able to report
#'   "another gender" for undergraduate students, even if you have no (or few)
#'   such students. Set as FALSE if necessary. Argument deprecated from the produce function.
#'
#' @importFrom rlang .data
#'
#' @importFrom dplyr select group_by summarize ungroup bind_rows arrange
#'   transmute n
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#' @keywords internal
#'
#
make_gr_part_E <- function(df, ugender) {

  lifecycle::deprecate_warn("2.9.0", "make_gr_part_E()",
                            details = "This survey no longer collects Gender Detail information; do not use this function.")

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partE <- df %>%
    #aggregate and count
    dplyr::group_by(.data$UNITID,
                    .data$GENDERDETAIL) %>%
    dplyr::summarize(COUNT = dplyr::n()) %>%
    dplyr::ungroup() %>%
    #add a column for pivoting
    mutate(GEN_COL = case_when(GENDERDETAIL == 1 ~ "Male",
                               GENDERDETAIL == 2 ~ "Female",
                               GENDERDETAIL == 3 ~ "GRGU011",
                               GENDERDETAIL == 4 ~ "GRGU012")) %>%
    select(-"GENDERDETAIL") %>%
    pivot_wider(names_from = "GEN_COL", values_from = "COUNT")

  #add missing columns as nulls, if they don't already exist in the data
  if (!"GRGU011" %in% names(partE)) {
    partE[["GRGU011"]] <- NA_integer_
  }
  if (!"GRGU012" %in% names(partE)) {
    partE[["GRGU012"]] <- NA_integer_
  }

  partE <- partE %>%
            #if ugender is true, we can report on another gender
    mutate(GRGU01 = case_when(ugender == TRUE ~ 1,
                              TRUE ~ 2),
           #if we have no unknown values, make it 0
           GRGU011 = dplyr::coalesce(.data$GRGU011, 0),
           #if we can report "another" show value; don't have any?, make it 0; otherwise -2
           GRGU012 = ifelse(ugender == TRUE & is.na(.data$GRGU012) == TRUE,
                            0,
                            ifelse(ugender == TRUE & !is.na(.data$GRGU012) == TRUE,
                                   .data$GRGU012,
                                   -2)
                            )
           ) %>%
    #now add 2023+ rules for masking
    mutate(GRGU012 = ifelse(.data$GRGU012 < 5, -2, .data$GRGU012),
           GRGU01 = ifelse(.data$GRGU012 < 5 & .data$GRGU01 == 1, 3, .data$GRGU01)
           ) %>%
    #set up final dataframe
    transmute(UNITID = unique(.data$UNITID),
              SURVSECT = "GR1",
              PART = "E",
              GRGU01 = .data$GRGU01,
              GRGU011 = .data$GRGU011,
              GRGU012 = .data$GRGU012)

    return(partE)
}
