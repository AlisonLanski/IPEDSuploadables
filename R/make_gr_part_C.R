#' Make Graduation Rates Part C
#'
#' @param df A dataframe of student/degree information
#'
#' @importFrom rlang .data
#' @importFrom dplyr select group_by summarize ungroup arrange transmute n
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A text file
#' @export
#'

make_gr_part_C <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #everyone
  partC_toline10 <- df %>%
                    dplyr::mutate(SECTION = case_when(
                                              .data$ENTERINGPROGRAMTYPE == 3 ~ 2,
                                              TRUE ~ 3
                                              ),
                                  LINE = 10) %>%
                    dplyr::select("UNITID",
                                  "PELLGRANT",
                                  "DIRECTLOAN",
                                  "SECTION",
                                  "LINE")

  #BA/nonBA completers by 150
  partC_toline18 <- df %>%
                    dplyr::filter(.data$COMPLETED150 == 1,
                                  .data$CURRENTPROGRAMTYPE == 3) %>%
                    dplyr::mutate(SECTION = case_when(
                                              .data$ENTERINGPROGRAMTYPE == 3 ~ 2,
                                              TRUE ~ 3
                                              ),
                                  LINE = 18) %>%
                    dplyr::select("UNITID",
                                  "PELLGRANT",
                                  "DIRECTLOAN",
                                  "SECTION",
                                  "LINE")

  #everyone by 150 and exclusions
  partC_toline45 <- df %>%
                    dplyr::mutate(SECTION = case_when(
                                                .data$ENTERINGPROGRAMTYPE == 3 ~ 2,
                                                TRUE ~ 3),
                                  LINE = case_when(.data$COMPLETED150 == 1 ~ 29,
                                                   .data$ISEXCLUSION == 1 ~ 45)
                                  ) %>%
                    dplyr::select("UNITID",
                                  "PELLGRANT",
                                  "DIRECTLOAN",
                                  "SECTION",
                                  "LINE")

  #put it together
  partC <- dplyr::bind_rows(partC_toline10,
                            partC_toline18,
                            partC_toline45) %>%
           #remove extraneous rows
           dplyr::filter((.data$PELLGRANT == 1 | .data$DIRECTLOAN == 1)) %>%
           dplyr::filter(!is.na(.data$LINE)) %>%
           #clean up Pell vs DIRECTLOAN
           dplyr::mutate(DIRECTLOANONLY = case_when(.data$PELLGRANT == 1 ~ 0,
                                                    .data$DIRECTLOAN == 1 ~ 1,
                                                    TRUE ~ 0)
                         ) %>%
           #aggregate and count
           dplyr::group_by(.data$UNITID,
                           .data$SECTION,
                           .data$LINE) %>%
           dplyr::summarize(TOTALPELL = sum(.data$PELLGRANT, na.rm = TRUE),
                            TOTALLOAN = sum(.data$DIRECTLOANONLY, na.rm = TRUE)) %>%
           dplyr::ungroup() %>%
           #format for upload
           dplyr::transmute(UNITID = .data$UNITID,
                            SURVSECT = "GR1",
                            PART = "C",
                            SECTION = .data$SECTION,
                            LINE = .data$LINE,
                            PELLGRANT_RCPT = .data$TOTALPELL,
                            DIRECTLOAN_RCPT = .data$TOTALLOAN
           )

}
