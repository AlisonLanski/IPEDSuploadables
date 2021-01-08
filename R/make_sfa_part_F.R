#' Make Student Financial Aid Part F
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


make_sfa_part_F <- function(df, extracips = NULL, output = "part", format = "both") {

  #produce the uploadable format
  partF <- df %>%
    #aggregate the full data
    dplyr::group_by(.data$Unitid, .data$IncomeLevel) %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=SFA",
                     YEAR = paste0("YEAR=", .data$Year),
                     INCOMELEVE = paste0("INCOMELEVE=", .data$IncomeLevel),
                     COUNT = paste0("COUNT=", .data$Count),
                     GCOUNT = paste0("GCOUNT=", .data$GCount),
                     AMOUNT = paste0("AMOUNT=", .data$Total)
                     )

  #create the txt file
  write_report(df = partF,
               component = "Financial Aid",
               part = "PartF",
               output = output,
               format = format)
}
