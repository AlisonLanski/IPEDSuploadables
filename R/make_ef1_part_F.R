#' Make Fall Enrollment Part F
#'
#' @description Student Faculty Ratio
#'
#' @param df A dataframe (either "students" or "retention") as a unitid source
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#'
#' @importFrom dplyr select group_by summarize arrange transmute n
#' @importFrom svDialogs dlg_input
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'

make_ef1_part_F <- function(df, output = "part", format = "both") {

  ratio <- svDialogs::dlg_input(default = 0, message = "Please enter the student number of your student to faculty ratio")$res

  partF <- data.frame(UNITID = paste0("UNITID=", get_ipeds_unitid(df)),
                      SURVSECT = "SURVSECT=EF1",
                      PART = "PART=F",
                      ST_STAFF_RATIO = paste0("ST_STAFF_RATIO=", ratio)
                     )

  #create the txt file
  write_report(df = partF,
               component = "FallEnrollment",
               part = "PartF",
               output = output,
               format = format)
}
