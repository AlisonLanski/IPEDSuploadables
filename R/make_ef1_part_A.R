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
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by summarize arrange transmute n mutate
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'


make_ef1_part_A <- function(df, cips = TRUE, output = "part", format = "both") {

  partA_prep <- df %>%
    dplyr::select(.data$Unitid,
                  .data$MajorCip,
                  .data$IsFullTime,
                  .data$IsFirstTime,
                  .data$IsTransfer,
                  .data$IsDegreeCertSeeking,
                  .data$StudentLevel,
                  .data$RaceEthnicity,
                  .data$Sex) %>%
    dplyr::mutate(Line = dplyr::case_when(
      .data$IsFullTime == 1 & .data$IsFirstTime == 1 & .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 1,
      .data$IsFullTime == 1 & .data$IsTransfer == 1 & .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 2,
      .data$IsFullTime == 1 & .data$IsFirstTime == 0 & .data$IsTransfer == 0 & .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 3,
      .data$IsFullTime == 1 & .data$IsDegreeCertSeeking == 0 & .data$StudentLevel == "Undergraduate" ~ 7,
      .data$IsFullTime == 1 & .data$StudentLevel == "Graduate" ~ 11,
      .data$IsFullTime == 0 & .data$IsFirstTime == 1 & .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 15,
      .data$IsFullTime == 0 & .data$IsTransfer == 1 & .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 16,
      .data$IsFullTime == 0 & .data$IsFirstTime == 0 & .data$IsTransfer == 0 & .data$IsDegreeCertSeeking == 1 & .data$StudentLevel == "Undergraduate" ~ 17,
      .data$IsFullTime == 0 & .data$IsDegreeCertSeeking == 0 ~ 21,
      .data$IsFullTime == 0 & .data$StudentLevel == "Graduate" ~ 25
    )
    )

  partA_all <- partA_prep %>%
    dplyr::mutate(MajorCip = '99.0000') %>%
    dplyr::group_by(.data$Unitid, .data$MajorCip, .data$Line, .data$RaceEthnicity, .data$Sex) %>%
    dplyr::summarise(Count = n()) %>%
    #sort for easy viewing
    dplyr::arrange(.data$Line, .data$RaceEthnicity, .data$Sex) %>%
    dplyr::ungroup()

  partA_cips <- partA_prep %>%
    dplyr::filter(.data$MajorCip %in% c('13.0000', '14.0000', '26.0000',
                                        '27.0000', '40.0000', '52.0000',
                                        '22.0101', '51.0401', '51.1201')) %>%
    dplyr::group_by(.data$Unitid, .data$MajorCip, .data$Line, .data$RaceEthnicity, .data$Sex) %>%
    dplyr::summarise(Count = n()) %>%
    #sort for easy viewing
    dplyr::arrange(.data$MajorCip, .data$Line, .data$RaceEthnicity, .data$Sex) %>%
    dplyr::ungroup()

  if(cips == TRUE){
    partA_ready <- rbind(partA_all, partA_cips)
  } else {
    partA_ready <- partA_all
  }

  partA <- partA_ready %>%

      #format for upload
      dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                       SURVSECT = "SURVSECT=EF1",
                       PART = "PART=A",
                       CIPCODE = paste0("CIPCODE=", .data$MajorCip),
                       LINE = paste0("LINE=", .data$Line),
                       RACE = paste0("RACE=", .data$RaceEthnicity),
                       SEX = paste0("SEX=", .data$Sex),
                       COUNT = paste0("COUNT=", .data$Count)
      )



  #create the txt file
  write_report(df = partA,
               component = "FallEnrollment",
               part = "PartA",
               output = output,
               format = format)
}
