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

  partE_counts <- df %>%
    dplyr::select("UNITID",
                  "STUDENTID",
                  "DEGREELEVEL",
                  "SEX"  #Binary = 1, 2;  Unknown = 3
                  ) %>%
    dplyr::filter(.data$SEX != 1 & .data$SEX != 2) %>%
    #break into UG and GR levels
    dplyr::mutate(UGPB = ifelse(.data$DEGREELEVEL %in% c(7, 8, 17, 18, 19), 'GR', 'UG')) %>%
    dplyr::select(-"DEGREELEVEL") %>%
    #deduplicate
    dplyr::distinct() %>%
    #aggregate and count
    dplyr::group_by(.data$UNITID,
                    .data$UGPB,
                    .data$SEX) %>%
    dplyr::summarize(COUNT = dplyr::n()) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$UGPB, .data$SEX)

  #set up the final DF
  partE <- data.frame(UNITID = unique(partE_counts$UNITID),
                      SURVSECT = "COM",
                      PART = "E",
                      CSEXUG = partE_counts$COUNT[partE_counts$UGPB == "UG"],
                      CSEXG = partE_counts$COUNT[partE_counts$UGPB == "GR"])

return(partE)

}



