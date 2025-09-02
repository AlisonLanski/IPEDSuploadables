#' Make Fall Enrollment Part H (Sex Unknown)
#'
#' @param df A dataframe of student enrollment information
#' @param ugender `r lifecycle::badge("deprecated")` A boolean: TRUE means you are collecting and able to report
#'   "another gender" for undergraduate students, even if you have no (or few)
#'   such students. Set as FALSE if necessary. **Starting in 2025-2026, this argument will be ignored by later
#'   code.**
#' @param ggender `r lifecycle::badge("deprecated")` A boolean: TRUE means you are collecting and able to report
#'   "another gender" for graduate students, even if you have no (or few) such
#'   students. Set as FALSE if necessary. **Starting in 2025-2026, this argument will be ignored by later
#'   code.**
#'
#' @importFrom rlang .data
#' @importFrom dplyr select group_by summarize ungroup arrange transmute n
#'   distinct
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_ef1_part_H <- function(df, ugender = lifecycle::deprecated(), ggender = lifecycle::deprecated()) {

  if (lifecycle::is_present(ugender)) {
    lifecycle::deprecate_warn(
      when = "2.11.0",
      what = "make_ef1_part_H(ugender)",
      details = "Detailed gender reporting is no longer used for this IPEDS survey. Argument may be removed in future versions."
    )
  }

  if (lifecycle::is_present(ggender)) {
    lifecycle::deprecate_warn(
      when = "2.11.0",
      what = "make_ef1_part_H(ggender)",
      details = "Detailed gender reporting is no longer used for this IPEDS survey. Argument may be removed in future versions."
    )
  }

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partH_unk <- df %>%
    dplyr::select("UNITID",
                  "STUDENTID",
                  "STUDENTLEVEL",
                  "GENDERDETAIL"  #Binary = 1, 2; Unknown = 3
    ) %>%

    #deduplicate
    dplyr::distinct() %>%

    #set up a single value for UNK -- will document -- anything NOT 1 or 2 will be counted as UNK for now
    dplyr::mutate(COUNT_UNK = case_when(.data$GENDERDETAIL == 1 ~ 'known',
                                        .data$GENDERDETAIL == 2 ~ 'known',
                                        TRUE ~ 'unknown')) %>%
    filter(.data$COUNT_UNK == 'unknown')

  if(nrow(partH_unk) > 0){
    partH <- partH_unk %>%
      #aggregate, count, reshape
      dplyr::group_by(.data$UNITID,
                      .data$STUDENTLEVEL) %>%
      dplyr::summarize(COUNT = dplyr::n()) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = "STUDENTLEVEL", values_from = "COUNT") %>%

      #add rows for a level if they are missing
      dplyr::bind_rows(dplyr::tibble(Undergraduate=numeric(), Graduate=numeric())) %>%

      #final DF
      dplyr::transmute(UNITID = get_ipeds_unitid(df),
                SURVSECT = "EF1",
                PART = "H",
                EFSEXUG = dplyr::coalesce(.data$Undergraduate, 0),
                EFSEXG = dplyr::coalesce(.data$Graduate, 0))
  } else {
    partH <- data.frame(UNITID = get_ipeds_unitid(df),
                        SURVSECT = "EF1",
                        PART = "H",
                        EFSEXUG = 0,
                        EFSEXG = 0)
  }

  return(partH)
}



