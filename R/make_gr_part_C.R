#' Make Graduation Rates Part C
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom dplyr select group_by summarize ungroup arrange transmute n
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A text file
#' @export
#'

make_gr_part_C <- function(df, output = "part", format = "both") {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #everyone
  partC_toline10 <- df %>%
                    dplyr::mutate(SECTION = case_when(
                                              .data$ENTERINGPROGRAMTYPE == 3 ~ 2,
                                              TRUE ~ 3
                                              ),
                                  LINE = 10) %>%
                    dplyr::select(.data$UNITID,
                                  .data$PELLGRANT,
                                  .data$DIRECTLOAN,
                                  .data$SECTION,
                                  .data$LINE)

  #BA/nonBA completers by 150
  partC_toline18 <- df %>%
                    dplyr::filter(.data$COMPLETED150 == 1,
                                  .data$CURRENTPROGRAMTYPE == 3) %>%
                    dplyr::mutate(SECTION = case_when(
                                              .data$ENTERINGPROGRAMTYPE == 3 ~ 2,
                                              TRUE ~ 3
                                              ),
                                  LINE = 18) %>%
                    dplyr::select(.data$UNITID,
                                  .data$PELLGRANT,
                                  .data$DIRECTLOAN,
                                  .data$SECTION,
                                  .data$LINE)

  #everyone by 150 and exclusions
  partC_toline45 <- df %>%
                    dplyr::mutate(SECTION = case_when(
                                                .data$ENTERINGPROGRAMTYPE == 3 ~ 2,
                                                TRUE ~ 3),
                                  LINE = case_when(.data$COMPLETED150 == 1 ~ 29,
                                                   .data$ISEXCLUSION == 1 ~ 45)
                                  ) %>%
                    dplyr::select(.data$UNITID,
                                  .data$PELLGRANT,
                                  .data$DIRECTLOAN,
                                  .data$SECTION,
                                  .data$LINE)

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
           dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                            SURVSECT = "SURVSECT=GR1",
                            PART = "PART=C",
                            SECTION = paste0("SECTION=", .data$SECTION),
                            LINE = paste0("LINE=", .data$LINE),
                            PELLGRANT_RCPT = paste0("PELLGRANT_RCPT=", .data$TOTALPELL),
                            DIRECTLOAN_RCPT = paste0("DIRECTLOAN_RCPT=", .data$TOTALLOAN)
           )

  write_report(df = partC,
               component = "GradRates",
               part = "PartC",
               output = output,
               format = format)
}
