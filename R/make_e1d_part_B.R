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
#' @importFrom stringr str_to_upper
#'
#' @return A text file
#' @export
#'

make_e1d_part_B <- function(df, output = "part", format = "both") {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partB <- df %>%
          #format for upload
          dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                           SURVSECT = "SURVSECT=E1D",
                           PART = "PART=B",
                           CREDHRSU = paste0("CREDHRSU=", .data$CREDITHOURSUG),
                           CONTHRS = paste0("CONTHRS=", .data$CLOCKHOURSUG),
                           CREDHRSG = paste0("CREDHRSG=", .data$CREDITHOURSGR),
                           RDOCFTE = paste0("RDOCFTE=", .data$DOCFTE)
          )

  #create the txt file
  write_report(df = partB,
               component = "12MonthEnrollment",
               part = "PartB",
               output = output,
               format = format)
}
