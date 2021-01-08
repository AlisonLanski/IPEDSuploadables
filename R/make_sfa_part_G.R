#' Make Student Financial Aid Part G
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


make_sfa_part_G <- function(df, extracips = NULL, output = "part", format = "both") {

  #produce the uploadable format
  partG <- df %>%
    #aggregate the full data
    dplyr::group_by(.data$Unitid, .data$StudentLevel) %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=SFA",
                     PART = "PART=G",
                     SLEVEL = paste0("SLEVEL=", .data$StudentLevel),
                     GI_BEN_N	= paste0("GI_BEN_N=", .data$GIBenNumber),
                     GI_BEN_T = paste0("GI_BEN_T=", .data$GIBenTotal),
                     DOD_ASSIST_N = paste0("DOD_ASSIST_N=", .data$DODNumber),
                     DOD_ASSIST_T = paste0("DOD_ASSIST_T=", .data$DODTotal)
                     )

  #create the txt file
  write_report(df = partG,
               component = "Financial Aid",
               part = "PartG",
               output = output,
               format = format)
}
