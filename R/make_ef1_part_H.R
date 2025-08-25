#' Make Fall Enrollment Part H (Sex Unknown)
#'
#' @param df A dataframe of student enrollment information
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

  partH_counts <- df %>%
    dplyr::select("UNITID",
                  "STUDENTID",
                  "STUDENTLEVEL",
                  "SEX"  #Binary = 1, 2
    ) %>%
    dplyr::filter(.data$SEX != 1 & .data$SEX != 2) %>%
    #break into UG and GR levels
    dplyr::mutate(UGPB = ifelse(.data$STUDENTLEVEL == 'Graduate', 'GR', 'UG')) %>%
    dplyr::select(-"STUDENTLEVEL") %>%
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
  partH <- data.frame(UNITID = unique(partH_counts$UNITID),
                      SURVSECT = "EF1",
                      PART = "H",
                      EFSEXUG = partH_counts$COUNT[partH_counts$UGPB == "UG"],
                      EFSEXG = partH_counts$COUNT[partH_counts$UGPB == "GR"])


    #BUT -- New in 2023 - mask if < 5 and set initial inquiry as "small N"
    if(partH$EFGU022 < 5){
      partH$EFGU022 <- -2
      partH$EFGU02 <- 3
    }

  return(partH)

}



