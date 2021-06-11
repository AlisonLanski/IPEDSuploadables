#' Make Fall Enrollment Part F
#'
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by summarize arrange transmute n
#' @importFromt svDialogs dlg_input
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'


make_ef1_part_F <- function(output = "part", format = "both") {

  ratio <- svDialogs::dlg_input(default = 0, message = "Please enter the student number of your student to faculty ratio")$res

  partF <- dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
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
