#' Make Outcome Measures Part A
#'
#' @description Establishing the Outcome Measures cohorts
#'
#' @param df A dataframe of student statuses
#'
#' @importFrom dplyr group_by summarize ungroup arrange transmute n
#' @importFrom stringr str_to_upper
#'
#' @return A text file ready for IPEDS upload
#' @export
#'

make_om_part_A <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #make sure we will end up with all possible reporting combinations
  dummyA <- expand.grid(UNITID = get_ipeds_unitid(df),
                        COHORTTYPE = c(1, 2, 3, 4),
                        RECIPIENT = c(1, 2),
                        COUNTED = 0,
                        EXCLUSION = FALSE,
                        stringsAsFactors = FALSE)

  partA <- df %>%
          #add extra rows
          dplyr::mutate(COUNTED = 1) %>%
          dplyr::bind_rows(dummyA) %>%

          #aggregate the full data
          dplyr::group_by(.data$UNITID,
                          .data$COHORTTYPE,
                          .data$RECIPIENT)%>%
          dplyr::summarize(COHORTCOUNT = sum(.data$COUNTED),
                           EXCLUSIONCOUNT = sum(.data$EXCLUSION)) %>%
          dplyr::ungroup() %>%

          #sort for easy viewing
          dplyr::arrange(.data$COHORTTYPE,
                         .data$RECIPIENT) %>%

          #format for upload
          dplyr::transmute(UNITID = .data$UNITID,
                           SURVSECT = 'OM1',
                           PART = 'A',
                           LINE = .data$COHORTTYPE,
                           RECIPIENT_TYPE = .data$RECIPIENT,
                           COHORT = .data$COHORTCOUNT,
                           EXCLUSION = .data$EXCLUSIONCOUNT)

}
