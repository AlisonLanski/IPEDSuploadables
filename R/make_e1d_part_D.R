#' Make 12 Month Enrollment Part D (gender details)
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
#'
#' @importFrom dplyr select group_by summarize ungroup bind_rows arrange
#'   transmute n
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

  partD_counts <- df %>%
    dplyr::select("UNITID",
                  "STUDENTID",
                  "STUDENTLEVEL",
                  "GENDERDETAIL") %>%
    #deduplicate
    dplyr::distinct() %>%
    #aggregate and count
    dplyr::group_by(.data$UNITID,
                    .data$STUDENTLEVEL,
                    .data$GENDERDETAIL) %>%
    dplyr::filter(.data$GENDERDETAIL != 1 & .data$GENDERDETAIL != 2) %>%
    dplyr::summarize(COUNT = dplyr::n()) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$STUDENTLEVEL, .data$GENDERDETAIL)


    #set up the final DF
    partD <- data.frame(UNITID = unique(partD_counts$UNITID),
                        SURVSECT = "E1D",
                        PART = "D",
                        FYSEXUG = partD_counts$COUNT[partD_counts$UGPB == "UG"],
                        FYSEXG = partD_counts$COUNT[partD_counts$UGPB == "GR"])

    return(partD)
}
