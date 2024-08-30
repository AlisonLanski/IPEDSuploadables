#' Make Fall Enrollment Part C
#'
#' @description State of origin for first time students
#'
#' @param df A dataframe of student/degree information
#'
#' @importFrom rlang .data
#'
#' @importFrom dplyr select group_by ungroup bind_rows arrange transmute n mutate bind_rows
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_ef1_part_C <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partC_all <- df %>%
               dplyr::select("UNITID",
                             "ISFIRSTTIME",
                             "ISDEGREECERTSEEKING",
                             "STUDENTLEVEL",
                             "ADMITSTATE") %>%
               dplyr::filter(.data$ISFIRSTTIME == 1,
                             .data$ISDEGREECERTSEEKING == 1,
                             .data$STUDENTLEVEL == "Undergraduate") %>%
               dplyr::mutate(LINE = .data$ADMITSTATE,
                             HS = 1) %>%
               dplyr::group_by(.data$UNITID,
                               .data$LINE,
                               .data$HS) %>%
               dplyr::summarize(COUNT = n()) %>%
               dplyr::ungroup()


  #have to do this separate because we DO want to double-COUNT the recent grads in both parts
  partC_recent <- df %>%
                  dplyr::select("UNITID",
                                "ISFIRSTTIME",
                                "ISDEGREECERTSEEKING",
                                "STUDENTLEVEL",
                                "ADMITSTATE",
                                "ISRECENTGRAD") %>%
                  dplyr::filter(.data$ISFIRSTTIME == 1,
                                .data$ISDEGREECERTSEEKING == 1,
                                .data$STUDENTLEVEL == "Undergraduate",
                                .data$ISRECENTGRAD == 1) %>%
                  dplyr::mutate(LINE = .data$ADMITSTATE,
                                HS = 2) %>%
                  dplyr::group_by(.data$UNITID,
                                  .data$LINE,
                                  .data$HS) %>%
                  dplyr::summarize(COUNT = n()) %>%
                  dplyr::ungroup()

  #put them together
  partC <- dplyr::bind_rows(partC_all,
                            partC_recent) %>%
           #remove the unknown-unknowns because the form will calc that for us
           dplyr::filter(.data$LINE != 99) %>%
           #sort for easy viewing
           dplyr::arrange(.data$LINE,
                          .data$HS) %>%
           #format for upload
           dplyr::transmute(UNITID = .data$UNITID,
                            SURVSECT = "EF1",
                            PART = "C",
                            LINE = .data$LINE,
                            HS = .data$HS,
                            COUNT = .data$COUNT
           )

}
