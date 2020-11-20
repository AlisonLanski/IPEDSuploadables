#' Make Graduation Rates 200
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by summarize ungroup bind_rows arrange transmute n
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'


make_gr200 <- function(df, output = "part", format = "both") {

  #produce the uploadable format
  exclusions <- df %>%
    dplyr::filter(.data$IsExclusion == 1) %>%
    dplyr::group_by(.data$IsExclusion) %>%
    dplyr::summarize(Exclusions = dplyr::n()) %>%
    ungroup()

  comp <- df %>%
    dplyr::filter(.data$IsComp == 1) %>%
    dplyr::group_by(.data$IsComp) %>%
    dplyr::summarize(Comp = dplyr::n()) %>%
    ungroup()

  se <- df %>%
    dplyr::filter(.data$IsStillEnrolled == 1) %>%
    dplyr::group_by(.data$IsStillEnrolled) %>%
    dplyr::summarize(StillEnrolled = dplyr::n()) %>%
    ungroup()

  gr200 <- df %>%
    dplyr::select(.data$Unitid) %>%
    dplyr::distinct() %>%
    dplyr::bind_cols(exclusions, comp, se) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=GR21",
                     PART = "PART=A",
                     ADEXCL = paste0("ADEXCL=", .data$Exclusions),
                     `COMPY7-8` = paste0("COMPY7-8=", .data$Comp),
                     STILLENROLLED = paste0("STILLENROLLED=", .data$StillEnrolled),
                     )

  #create the txt file
  write_report(df = gr200,
               component = "GR200",
               part = "AllData",
               output = output,
               format = format)
}
