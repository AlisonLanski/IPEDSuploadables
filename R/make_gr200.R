#' Make Graduation Rates 200
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by summarize ungroup transmute
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'


make_gr200 <- function(df, output = "part", format = "both") {

  gr200 <- df %>%
    dplyr::group_by(.data$Unitid) %>%
    dplyr::summarize(Exclusions = sum(.data$IsExclusion),
                    Comp = sum(.data$IsComp),
                    StillEnrolled = sum(.data$IsStillEnrolled)) %>%
    dplyr::ungroup() %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=G21",
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
