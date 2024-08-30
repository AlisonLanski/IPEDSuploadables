#' Make 12 Month Enrollment Part B
#'
#' @param df A dataframe with summarized credit hours and student information
#'
#' @importFrom rlang .data
#'
#' @importFrom dplyr select group_by summarize ungroup bind_rows arrange transmute n
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_e1d_part_B <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partB <- df %>%
          #format for upload
          dplyr::transmute(UNITID = .data$UNITID,
                           SURVSECT = "E1D",
                           PART = "B",
                           CREDHRSU = .data$CREDITHOURSUG,
                           CONTHRS = .data$CLOCKHOURSUG,
                           CREDHRSG =.data$CREDITHOURSGR,
                           RDOCFTE = .data$DOCFTE
          )

}
