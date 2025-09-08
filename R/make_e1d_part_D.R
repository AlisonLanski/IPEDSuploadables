#' Make 12 Month Enrollment Part D (gender details)
#'
#' @param df A dataframe of student/degree information
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
#'
#' @importFrom dplyr select distinct mutate group_by summarize ungroup transmute n
#' @importFrom tidyr pivot_wider
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_e1d_part_D <- function(df, ugender = lifecycle::deprecated(), ggender = lifecycle::deprecated()) {

  if (lifecycle::is_present(ugender)) {
    lifecycle::deprecate_warn(
      when = "2.11.0",
      what = "make_e1d_part_D(ugender)",
      details = "Detailed gender reporting is no longer used for this IPEDS survey. Argument may be removed in future versions."
    )
  }

  if (lifecycle::is_present(ggender)) {
    lifecycle::deprecate_warn(
      when = "2.11.0",
      what = "make_e1d_part_D(ggender)",
      details = "Detailed gender reporting is no longer used for this IPEDS survey. Argument may be removed in future versions."
    )
  }

  colnames(df) <- stringr::str_to_upper(colnames(df))
#
#   partD_dummy <- data.frame(UNITID = get_ipeds_unitid(df),
#                             STUDENTLEVEL = c('Undergraduate', 'Graduate'),
#                             COUNT_UNK = 0)

  partD_unk <- df %>%
    dplyr::select("UNITID",
                  "STUDENTID",
                  "STUDENTLEVEL",
                  "GENDERDETAIL") %>%
    #deduplicate
    dplyr::distinct() %>%

    #set up a single value for UNK -- will document -- anything NOT 1 or 2 will be counted as UNK for now
    dplyr::mutate(COUNT_UNK = case_when(.data$GENDERDETAIL == 1 ~ 'known',
                                  .data$GENDERDETAIL == 2 ~ 'known',
                                  TRUE ~ 'unknown')) %>%
    filter(.data$COUNT_UNK == 'unknown')

  if(nrow(partD_unk) > 0){
    partD <- partD_unk %>%
      #aggregate, count, reshape
      dplyr::group_by(.data$UNITID,
                      .data$STUDENTLEVEL) %>%
      dplyr::summarize(COUNT = dplyr::n()) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = "STUDENTLEVEL", values_from = "COUNT") %>%

      #add rows for a level if they are missing
      dplyr::bind_rows(dplyr::tibble(Undergraduate=numeric(), Graduate=numeric())) %>%

      #final DF
      dplyr::transmute(UNITID = .data$UNITID,
                       SURVSECT = "E1D",
                       PART = "D",
                       FYSEXUG = dplyr::coalesce(.data$Undergraduate, 0),
                       FYSEXG = dplyr::coalesce(.data$Graduate, 0))
  }


  else{
    partD <- data.frame(UNITID = get_ipeds_unitid(df),
                        SURVSECT = "E1D",
                        PART = "D",
                        FYSEXUG = 0,
                        FYSEXG = 0)
  }



    return(partD)
}
