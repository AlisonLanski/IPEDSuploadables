#' Make Fall Enrollment Part A
#'
#' @description Breakdown of students level and demographics; also by designated CIPs in required years
#'
#' @param df A dataframe of student information
#' @param cips A logical indicating if part A  needs to provide breakdowns by particular CIPs
#'
#' @importFrom rlang .data
#'
#' @importFrom dplyr select group_by summarize arrange transmute n mutate bind_rows
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A text file
#' @export
#'

make_ef1_part_A <- function(df, cips = TRUE) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partA_prep <- df %>%
                dplyr::select("UNITID",
                              "ISFULLTIME",
                              "ISFIRSTTIME",
                              "ISTRANSFER",
                              "ISDEGREECERTSEEKING",
                              "STUDENTLEVEL",
                              "RACEETHNICITY",
                              "SEX",
                              "CIP130000",
                              "CIP140000",
                              "CIP260000",
                              "CIP270000",
                              "CIP400000",
                              "CIP520000",
                              "CIP220101",
                              "CIP510401",
                              "CIP511201") %>%
                dplyr::mutate(LINE = dplyr::case_when(
                                      .data$ISFULLTIME == 1 & .data$ISFIRSTTIME == 1 & .data$ISDEGREECERTSEEKING == 1 & .data$STUDENTLEVEL == "Undergraduate" ~ 1,
                                      .data$ISFULLTIME == 1 & .data$ISTRANSFER == 1 & .data$ISDEGREECERTSEEKING == 1 & .data$STUDENTLEVEL == "Undergraduate" ~ 2,
                                      .data$ISFULLTIME == 1 & .data$ISFIRSTTIME == 0 & .data$ISTRANSFER == 0 & .data$ISDEGREECERTSEEKING == 1 & .data$STUDENTLEVEL == "Undergraduate" ~ 3,
                                      .data$ISFULLTIME == 1 & .data$ISDEGREECERTSEEKING == 0 & .data$STUDENTLEVEL == "Undergraduate" ~ 7,
                                      .data$ISFULLTIME == 1 & .data$STUDENTLEVEL == "Graduate" ~ 11,
                                      .data$ISFULLTIME == 0 & .data$ISFIRSTTIME == 1 & .data$ISDEGREECERTSEEKING == 1 & .data$STUDENTLEVEL == "Undergraduate" ~ 15,
                                      .data$ISFULLTIME == 0 & .data$ISTRANSFER == 1 & .data$ISDEGREECERTSEEKING == 1 & .data$STUDENTLEVEL == "Undergraduate" ~ 16,
                                      .data$ISFULLTIME == 0 & .data$ISFIRSTTIME == 0 & .data$ISTRANSFER == 0 & .data$ISDEGREECERTSEEKING == 1 & .data$STUDENTLEVEL == "Undergraduate" ~ 17,
                                      .data$ISFULLTIME == 0 & .data$ISDEGREECERTSEEKING == 0  & .data$STUDENTLEVEL == "Undergraduate" ~ 21,
                                      .data$ISFULLTIME == 0 & .data$STUDENTLEVEL == "Graduate" ~ 25
                                    )
                              )

  partA_all <- partA_prep %>%
               dplyr::mutate(MAJORCIP = "99.0000") %>%
               dplyr::group_by(.data$UNITID,
                               .data$MAJORCIP,
                               .data$LINE,
                               .data$RACEETHNICITY,
                               .data$SEX) %>%
               dplyr::summarize(COUNT = n()) %>%
               #sort for easy viewing
               dplyr::arrange(.data$LINE,
                              .data$RACEETHNICITY,
                              .data$SEX) %>%
               dplyr::ungroup()


  if(cips == TRUE){
    partA_cips <- partA_prep %>%
      tidyr::pivot_longer(cols = dplyr::matches("CIP"), names_to = "MAJORCIP", values_to = "CipFlag") %>%
      dplyr::filter(.data$CipFlag == 1) %>%
      dplyr::mutate(MAJORCIP = paste0(substr(.data$MAJORCIP, 4, 5), ".", substring(.data$MAJORCIP, 6, 9))) %>%
      dplyr::group_by(.data$UNITID,
                      .data$MAJORCIP,
                      .data$LINE,
                      .data$RACEETHNICITY,
                      .data$SEX) %>%
      dplyr::summarize(COUNT = n()) %>%
      #sort for easy viewing
      dplyr::arrange(.data$MAJORCIP,
                     .data$LINE,
                     .data$RACEETHNICITY,
                     .data$SEX) %>%
      dplyr::ungroup()
    partA_ready <- dplyr::bind_rows(partA_all,
                                    partA_cips)
  } else {
    partA_ready <- partA_all
  }

  partA <- partA_ready %>%
      #format for upload
      dplyr::transmute(UNITID = .data$UNITID,
                       SURVSECT = "EF1",
                       PART = "A",
                       CIPCODE = .data$MAJORCIP,
                       LINE = .data$LINE,
                       RACE = .data$RACEETHNICITY,
                       SEX = .data$SEX,
                       COUNT = .data$COUNT
                      )

}
