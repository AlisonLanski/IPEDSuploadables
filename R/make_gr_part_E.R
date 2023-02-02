#' Make Graduation Rates Part E (gender details)
#'
#' @param df A dataframe of student/degree information for unduplicated undergraduates
#' @param ugender A boolean: TRUE means you are collecting and able to report
#'   "another gender" for undergraduate students. Set as FALSE if necessary
#'
#' @importFrom rlang .data
#'
#' @importFrom dplyr select group_by summarize ungroup bind_rows arrange transmute n
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A df aggregated for the survey part
#' @export
#'

make_gr_part_E <- function(df, ugender) {

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
    select(-.data$GENDERDETAIL) %>%
    pivot_wider(names_from = .data$GEN_COL, values_from = .data$COUNT)

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
    #set up final dataframe
    transmute(UNITID = unique(.data$UNITID),
              SURVSECT = "GR1",
              PART = "E",
              GRGU01 = .data$GRGU01,
              GRGU011 = .data$GRGU011,
              GRGU012 = .data$GRGU012)

    return(partE)
}
