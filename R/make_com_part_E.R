#' Make Completions Part E (gender details)
#'
#' @param df A dataframe of student/degree information
#' @param ugender `r lifecycle::badge("deprecated")` A boolean: TRUE means you are collecting and able to report
#'   "another gender" for undergraduate completers, even if you have no (or few)
#'   such students. Set as FALSE if necessary. **Starting in 2025-2026, this argument will be ignored by later
#'   code.**
#' @param ggender `r lifecycle::badge("deprecated")` A boolean: TRUE means you are collecting and able to report
#'   "another gender" for graduate completers, even if you have no (or few) such
#'   students. Set as FALSE if necessary. **Starting in 2025-2026, this argument will be ignored by later
#'   code.**
#'
#' @importFrom rlang .data
#' @importFrom dplyr select group_by summarize ungroup arrange transmute n distinct
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_com_part_E <- function(df, ugender = lifecycle::deprecated(), ggender = lifecycle::deprecated()) {

  if (lifecycle::is_present(ugender)) {
    lifecycle::deprecate_warn(
      when = "2.11.0",
      what = "make_com_part_E(ugender)",
      details = "Detailed gender reporting is no longer used for this IPEDS survey. Argument may be removed in future versions."
    )
  }

  if (lifecycle::is_present(ggender)) {
    lifecycle::deprecate_warn(
      when = "2.11.0",
      what = "make_com_part_E(ggender)",
      details = "Detailed gender reporting is no longer used for this IPEDS survey. Argument may be removed in future versions."
    )
  }

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partE_unk <- df %>%
    dplyr::select("UNITID",
                  "STUDENTID",
                  "DEGREELEVEL",
                  "GENDERDETAIL"  #Binary = 1, 2;  Unknown = 3
                  ) %>%
    #set up a single value for UNK -- will document -- anything NOT 1 or 2 will be counted as UNK for now
    dplyr::mutate(COUNT_UNK = case_when(.data$GENDERDETAIL == 1 ~ 'known',
                                        .data$GENDERDETAIL == 2 ~ 'known',
                                        TRUE ~ 'unknown')) %>%
    filter(.data$COUNT_UNK == 'unknown')

  if(nrow(partE_unk) > 0){
    partE <- partE_unk %>%
      #break into UG and GR levels
      dplyr::mutate(UGPB = ifelse(.data$DEGREELEVEL %in% c(7, 8, 17, 18, 19), 'GR', 'UG')) %>%
      dplyr::select(-"DEGREELEVEL") %>%
      #deduplicate
      dplyr::distinct() %>%

      #aggregate, count, reshape
      dplyr::group_by(.data$UNITID,
                      .data$UGPB) %>%
      dplyr::summarize(COUNT = dplyr::n()) %>%
      dplyr::ungroup() %>%
      #sort for easy viewing
      dplyr::arrange(.data$UGPB) %>%
      tidyr::pivot_wider(names_from = "UGPB", values_from = "COUNT") %>%

      #add rows for a level if they are missing
      dplyr::bind_rows(dplyr::tibble(UG=numeric(), GR=numeric())) %>%

      #set up the final DF
      transmute(.data$UNITID,
                SURVSECT = "COM",
                PART = "E",
                CSEXUG = dplyr::coalesce(.data$UG, 0),
                CSEXG = dplyr::coalesce(.data$GR, 0))
  } else {
    partE <- data.frame(UNITID = get_ipeds_unitid(df),
                        SURVSECT = "COM",
                        PART = "E",
                        CSEXUG = 0,
                        CSEXG = 0)
  }



return(partE)
}



