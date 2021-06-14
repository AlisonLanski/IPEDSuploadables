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
#'
#' @return A text file
#' @export
#'


make_e1d_part_C <- function(df, output = "part", format = "both") {

  partC <- df %>%
    dplyr::select(.data$Unitid,
                  .data$StudentID,
                  .data$IsDegreeCertSeeking,
                  .data$StudentLevel,
                  .data$DistanceEdAll,
                  .data$DistanceEdSome) %>%
    dplyr::mutate(Line = case_when(
                                  .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 1,
                                  .data$IsDegreeCertSeeking == 0 & .data$StudentLevel == "Undergraduate" ~ 2,
                                  .data$StudentLevel == "Graduate" ~ 3
                                )
                  ) %>%
    dplyr::select(-c(.data$IsDegreeCertSeeking, .data$StudentLevel)) %>%
    dplyr::group_by(.data$Unitid, .data$Line) %>%
    dplyr::summarise(CountDistanceEdAll = sum(as.numeric(.data$DistanceEdAll)),
                     CountDistanceEdSome = sum(as.numeric(.data$DistanceEdSome))
                     ) %>%
    dplyr::ungroup() %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=E1D",
                     PART = "PART=C",
                     LINE = paste0("LINE=", .data$Line),
                     ENROLL_EXCLUSIVE = paste0("ENROLL_EXCLUSIVE=", .data$CountDistanceEdAll),
                     ENROLL_SOME = paste0("ENROLL_SOME=", .data$CountDistanceEdSome)
                    )

  #create the txt file
  write_report(df = partC,
               component = "12MonthEnrollment",
               part = "PartC",
               output = output,
               format = format)
}
