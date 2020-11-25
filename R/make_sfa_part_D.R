#' Make Student Financial Aid Part D
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


make_sfa_part_D <- function(df, extracips = NULL, output = "part", format = "both") {

  #produce the uploadable format
  partD <- df %>%
    #aggregate the full data
    dplyr::group_by(.data$Unitid, .data$Linetype, .data$Yearcode) %>%
    dplyr::summarize(Total = sum(.data$Total)) %>%
    dplyr::ungroup() %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=SFA",
                     LINETYPE = paste0("LINETYPE=", .data$Linetype),
                     YEARCODE = paste0("YEARCODE=", .data$Yearcode),
                     AMOUNT = paste0("AMOUNT=", .data$Total)
    )

  #create the txt file
  write_report(df = partD,
               component = "Financial Aid",
               part = "PartD",
               output = output,
               format = format)
}
