#' Make 12 Month Enrollment Part A
#'
#' @param df A dataframe of student/degree information
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


make_e1d_part_A <- function(df, output = "part", format = "both") {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partA <- df %>%
    dplyr::select(.data$UNITID,
                  .data$STUDENTID,
                  .data$ISFULLTIME,
                  .data$ISFIRSTTIME,
                  .data$ISTRANSFER,
                  .data$ISDEGREECERTSEEKING,
                  .data$STUDENTLEVEL,
                  .data$RACEETHNICITY,
                  .data$SEX) %>%
    dplyr::mutate(LINE = dplyr::case_when(
                                  .data$ISFULLTIME == 1 & .data$ISFIRSTTIME == 1 &
                                    .data$ISDEGREECERTSEEKING == 1 &
                                    .data$STUDENTLEVEL == "Undergraduate" ~ 1,

                                  .data$ISFULLTIME == 1 &
                                    .data$ISTRANSFER == 1 &
                                    .data$ISDEGREECERTSEEKING == 1 &
                                    .data$STUDENTLEVEL == "Undergraduate" ~ 2,

                                  .data$ISFULLTIME == 1 &
                                    .data$ISFIRSTTIME == 0 &
                                    .data$ISTRANSFER == 0 &
                                    .data$ISDEGREECERTSEEKING == 1 &
                                    .data$STUDENTLEVEL == "Undergraduate" ~ 3,

                                  .data$ISFULLTIME == 1 &
                                    .data$ISDEGREECERTSEEKING == 0 &
                                    .data$STUDENTLEVEL == "Undergraduate" ~ 7,

                                  .data$ISFULLTIME == 0 &
                                    .data$ISFIRSTTIME == 1 &
                                    .data$ISDEGREECERTSEEKING == 1 &
                                    .data$STUDENTLEVEL == "Undergraduate" ~ 15,

                                  .data$ISFULLTIME == 0 &
                                    .data$ISTRANSFER == 1 &
                                    .data$ISDEGREECERTSEEKING == 1 &
                                    .data$STUDENTLEVEL == "Undergraduate" ~ 16,

                                  .data$ISFULLTIME == 0 &
                                    .data$ISFIRSTTIME == 0 &
                                    .data$ISTRANSFER == 0 &
                                    .data$ISDEGREECERTSEEKING == 1 &
                                    .data$STUDENTLEVEL == "Undergraduate" ~ 17,

                                  .data$ISFULLTIME == 0 &
                                    .data$ISDEGREECERTSEEKING == 0 &
                                    .data$STUDENTLEVEL == "Undergraduate" ~ 21,

                                  .data$STUDENTLEVEL == "Graduate" ~ 99
                                )
                  ) %>%
    dplyr::group_by(.data$UNITID, .data$LINE, .data$RACEETHNICITY, .data$SEX) %>%
    dplyr::summarise(COUNT = n()) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$LINE, .data$RACEETHNICITY, .data$SEX) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                     SURVSECT = "SURVSECT=E1D",
                     PART = "PART=A",
                     LINE = paste0("LINE=", .data$LINE),
                     RACE = paste0("RACE=", .data$RACEETHNICITY),
                     SEX = paste0("SEX=", .data$SEX),
                     COUNT = paste0("COUNT=", .data$COUNT)
                    )

  #create the txt file
  write_report(df = partA,
               component = "12MonthEnrollment",
               part = "PartA",
               output = output,
               format = format)
}
