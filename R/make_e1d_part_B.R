#' Make 12 Month Enrollment Part B
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


make_e1d_part_B <- function(df, extracips = NULL, output = "part", format = "both") {

  partB <- partB %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=E1D",
                     PART = "PART=B",
                     CREDHRSU = paste0("CREDHRSU=", .data$CreditHours),
                     CONTHRS = paste0("CONTHRS=", .data$ClockHours),
                     CREDHRSG = paste0("CREDHRSG=", .data$CreditHours),
                     RDOCFTE = paste0("RDOCFTE=", .data$DocFTE)
    )

  #create the txt file
  write_report(df = partB,
               component = "12MonthEnrollment",
               part = "PartB",
               output = output,
               format = format)
}
