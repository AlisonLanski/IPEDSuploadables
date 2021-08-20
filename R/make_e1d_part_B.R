#' Make 12 Month Enrollment Part B
#'
#' @param df A dataframe with summarized credit hours and student information
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


make_e1d_part_B <- function(df, output = "part", format = "both") {

    partB <- df %>%
      #format for upload
      dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                       SURVSECT = "SURVSECT=E1D",
                       PART = "PART=B",
                       CREDHRSU = paste0("CREDHRSU=", .data$CreditHoursUG),
                       CONTHRS = paste0("CONTHRS=", .data$ClockHoursUG),
                       CREDHRSG = paste0("CREDHRSG=", .data$CreditHoursGR),
                       RDOCFTE = paste0("RDOCFTE=", .data$DocFTE)
      )


  #create the txt file
  write_report(df = partB,
               component = "12MonthEnrollment",
               part = "PartB",
               output = output,
               format = format)
}
