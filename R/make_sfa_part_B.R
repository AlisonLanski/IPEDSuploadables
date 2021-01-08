#' Make Student Financial Aid Part B
#'
#' @param df A dataframe of student/degree information
#' @param extracips A dataframe of cips offered by the institution but not in \code{'df'}
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


make_sfa_part_B <- function(df, extracips = NULL, output = "part", format = "both") {

  #produce the uploadable format
  partB <- df %>%
    #aggregate the full data
    dplyr::group_by(.data$Unitid, .data$Linetype) %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=SFA",
                     LINETYPE = paste0("LINETYPE=", .data$Linetype),
                     COUNT = paste0("COUNT=", .data$Count)
                    )

  #create the txt file
  write_report(df = partB,
               component = "Financial Aid",
               part = "PartB",
               output = output,
               format = format)
}
