#' Make Fall Enrollment Part D
#'
#' @description Count of new non-degree students
#'
#' @param df A dataframe of student/degree information
#'
#' @importFrom rlang .data
#'
#' @importFrom dplyr select group_by filter arrange transmute n
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_ef1_part_D <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partD <- df %>%
           dplyr::select("UNITID",
                         "ISDEGREECERTSEEKING",
                         "STUDENTLEVEL",
                         "ISFIRSTTIME",
                         "ISTRANSFER") %>%
           dplyr::mutate(NEWNONDEGREE = as.numeric(.data$ISDEGREECERTSEEKING == 0 &
                                                  .data$STUDENTLEVEL == "Undergraduate" &
                                                  (.data$ISFIRSTTIME == 1 | .data$ISTRANSFER == 1))) %>%
           dplyr::group_by(.data$UNITID) %>%
           dplyr::summarize(COUNT = sum(.data$NEWNONDEGREE, na.rm = T)) %>%
           dplyr::ungroup() %>%
           #format for upload
           dplyr::transmute(UNITID = .data$UNITID,
                            SURVSECT = "EF1",
                            PART = "D",
                            COUNT = .data$COUNT
                           )
}
