#' Make Graduation Rates 200
#'
#' @param df A dataframe of student/degree information
#'
#' @importFrom rlang .data
#'
#' @importFrom dplyr group_by summarize ungroup transmute
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'


make_gr200 <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  gr200 <- df %>%
           dplyr::group_by(.data$UNITID) %>%
           dplyr::summarize(EXCLUSIONS = sum(.data$ISEXCLUSION),
                            COMP = sum(.data$ISCOMP),
                            STILLENROLLED = sum(.data$ISSTILLENROLLED)) %>%
           dplyr::ungroup() %>%
           #format for upload
           dplyr::transmute(UNITID = .data$UNITID,
                            SURVSECT = "G21",
                            PART = "A",
                            ADEXCL = .data$EXCLUSIONS,
                            `COMPY7-8` = .data$COMP,
                            STILLENROLLED = .data$STILLENROLLED,
                            )

}
