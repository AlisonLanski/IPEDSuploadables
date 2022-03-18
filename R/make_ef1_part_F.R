#' Make Fall Enrollment Part F
#'
#' @description Student Faculty Ratio
#'
#' @param df A dataframe (either "students" or "retention") as a unitid source
#'
#' @importFrom svDialogs dlg_input
#'
#' @return A text file
#' @export
#'

make_ef1_part_F <- function(df) {

  ratio <- svDialogs::dlg_input(default = 0, message = "Please enter your student/faculty ratio as a whole number")$res

  partF <- data.frame(UNITID = get_ipeds_unitid(df),
                      SURVSECT = "EF1",
                      PART = "F",
                      ST_STAFF_RATIO = ratio
                     )

}
