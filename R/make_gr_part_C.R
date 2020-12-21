#' Make Graduation Rates Part C
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom dplyr select group_by summarize ungroup arrange transmute n
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'

make_gr_part_C <- function(df, output = "part", format = "both") {

  partC <- df %>%
    dplyr::select(.data$Unitid, .data$Section, .data$Line, .data$PellGrant, .data$DirectLoan) %>%
    #deduplicate
    dplyr::distinct() %>%
    #aggregate and count
    dplyr::group_by(.data$Unitid, .data$Section, .data$Line, .data$PellGrant, .data$DirectLoan) %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=GR1",
                     PART = "PART=C",
                     SECTION = paste0("SECTION=", .data$Section),
                     LINE = paste0("LINE=", .data$Line),
                     PELLGRANT_RCPT = paste0("SEX=", .data$PellGrant),
                     DIRECTLOAN_RCPT = paste0("DIRECTLOAN_RCPT=", .data$DirectLoan)
                     )

  write_report(df = partC,
               component = "GradRates",
               part = "PartC",
               output = output,
               format = format)
}
