#' Make Fall Enrollment Part A
#'
#' @description Breakdown of students level and demographics; also by designated CIPs in required years
#'
#' @param df A dataframe of student information
#' @param cips A logical indicating if part A  needs to provide breakdowns by particular CIPs
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
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

make_ef1_part_A <- function(df, cips = TRUE, output = "part", format = "both") {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partA_prep <- df %>%
                dplyr::select(.data$UNITID,
                              .data$MAJORCIP,
                              .data$ISFULLTIME,
                              .data$ISFIRSTTIME,
                              .data$ISTRANSFER,
                              .data$ISDEGREECERTSEEKING,
                              .data$STUDENTLEVEL,
                              .data$RACEETHNICITY,
                              .data$SEX) %>%
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
               dplyr::summarise(COUNT = n()) %>%
               #sort for easy viewing
               dplyr::arrange(.data$LINE,
                              .data$RACEETHNICITY,
                              .data$SEX) %>%
               dplyr::ungroup()

  partA_cips <- partA_prep %>%
                dplyr::filter(.data$MAJORCIP %in% c("13.0000", "14.0000", "26.0000",
                                                    "27.0000", "40.0000", "52.0000",
                                                    "22.0101", "51.0401", "51.1201")) %>%
                dplyr::group_by(.data$UNITID,
                                .data$MAJORCIP,
                                .data$LINE,
                                .data$RACEETHNICITY,
                                .data$SEX) %>%
                dplyr::summarise(COUNT = n()) %>%
                #sort for easy viewing
                dplyr::arrange(.data$MAJORCIP,
                               .data$LINE,
                               .data$RACEETHNICITY,
                               .data$SEX) %>%
                dplyr::ungroup()

  if(cips == TRUE){
    partA_ready <- dplyr::bind_rows(partA_all,
                                    partA_cips)
  } else {
    partA_ready <- partA_all
  }

  partA <- partA_ready %>%
      #format for upload
      dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                       SURVSECT = "SURVSECT=EF1",
                       PART = "PART=A",
                       CIPCODE = paste0("CIPCODE=", .data$MAJORCIP),
                       LINE = paste0("LINE=", .data$LINE),
                       RACE = paste0("RACE=", .data$RACEETHNICITY),
                       SEX = paste0("SEX=", .data$SEX),
                       COUNT = paste0("COUNT=", .data$COUNT)
                      )

  #create the txt file
  write_report(df = partA,
               component = "FallEnrollment",
               part = "PartA",
               output = output,
               format = format)
}
