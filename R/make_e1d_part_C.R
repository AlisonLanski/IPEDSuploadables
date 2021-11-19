#' Make 12 Month Enrollment Part C
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


make_e1d_part_C <- function(df, output = "part", format = "both") {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partC <- df %>%
    dplyr::select(.data$UNITID,
                  .data$STUDENTID,
                  .data$ISDEGREECERTSEEKING,
                  .data$STUDENTLEVEL,
                  .data$DISTANCEEDALL,
                  .data$DISTANCEEDSOME) %>%
    dplyr::mutate(LINE = case_when(
                                  .data$ISDEGREECERTSEEKING == 1 & .data$STUDENTLEVEL == "Undergraduate" ~ 1,
                                  .data$ISDEGREECERTSEEKING == 0 & .data$STUDENTLEVEL == "Undergraduate" ~ 2,
                                  .data$STUDENTLEVEL == "Graduate" ~ 3
                                )
                  ) %>%
    dplyr::select(-c(.data$ISDEGREECERTSEEKING, .data$STUDENTLEVEL)) %>%
    dplyr::group_by(.data$UNITID, .data$LINE) %>%
    dplyr::summarise(CountDISTANCEEDALL = sum(as.numeric(.data$DISTANCEEDALL)),
                     CountDISTANCEEDSOME = sum(as.numeric(.data$DISTANCEEDSOME))
                     ) %>%
    dplyr::ungroup() %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                     SURVSECT = "SURVSECT=E1D",
                     PART = "PART=C",
                     LINE = paste0("LINE=", .data$LINE),
                     ENROLL_EXCLUSIVE = paste0("ENROLL_EXCLUSIVE=", .data$CountDISTANCEEDALL),
                     ENROLL_SOME = paste0("ENROLL_SOME=", .data$CountDISTANCEEDSOME)
                    )

  #create the txt file
  write_report(df = partC,
               component = "12MonthEnrollment",
               part = "PartC",
               output = output,
               format = format)
}
