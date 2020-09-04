#' Make Completions Part D
#'
#' @param df A dataframe of student/degree information
#' @param extracips A dataframe of cips offered by the institution but not in \code{'df'}
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format
#'
#' @importFrom rlang .data
#' @importFrom dplyr select filter mutate bind_rows group_by ungroup summarize arrange everything recode
#' @importFrom tidyr spread
#' @importFrom svDialogs dlg_message
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'

make_com_part_D <- function(df, extracips = NULL, output = "part", format = "both") {

  if(!is.null(extracips)) {
    #check extracips list for award levels not included in the startingdf
    extralevel_D <- extracips %>%
      dplyr::select(.data$Unitid, .data$DegreeLevel) %>%
      unique() %>%
      dplyr::filter(!(.data$DegreeLevel %in% df$DegreeLevel)) %>%
      #add dummy data to any award levels found
      dplyr::mutate(StudentId = "dummy_studentid",
                    RaceEthnicity = 1,
                    Sex = 1,
                    Birthdate = lubridate::ymd("1900-01-01"),
                    CountRE = 0,
                    CountSex = 0,
                    CountAge = 0
      ) %>%
      #reorder for rbind
      dplyr::select(.data$Unitid, .data$StudentId, dplyr::everything())
  } else {
    extralevel_D <- data.frame(Unitid = df$Unitid[1],
                               DegreeLevel = df$DegreeLevel[1],
                               StudentId = "dummy_studentid",
                               RaceEthnicity = 1,
                               Sex = 1,
                               Birthdate = lubridate::ymd("1900-01-01"),
                               CountRE = 0,
                               CountSex = 0,
                               CountAge = 0)
  }

  #set up an df with 0-rows to ensure we get all
  #race/ethnicity, sex, and age categories in the final output

  ipeds_unitid <- as.character(get_ipeds_unitid(df))

  dummy_demographics <- data.frame(Unitid = ipeds_unitid,
                                   StudentId = "dummy_studentid",
                                   DegreeLevel = max(df$DegreeLevel),
                                   RaceEthnicity = c(1:9),
                                   Sex = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
                                   Age = c(15, 20, 25, 30, 35, 40, 45, 50, NA),
                                   CountRE = 0,
                                   CountSex = 0,
                                   CountAge = 0,
                                   stringsAsFactors = FALSE)

  partD <- df %>%
    dplyr::select(.data$Unitid, .data$StudentId, .data$DegreeLevel, .data$RaceEthnicity, .data$Sex, .data$Age) %>%
    #add values which will be summed later
    dplyr::mutate(CountRE = 1,
                  CountSex = 1,
                  CountAge = 1
    ) %>%
    #add any extra award levels
    dplyr::bind_rows(extralevel_D) %>%
    #add dummy demographics to make sure the spread works correctly later
    dplyr::bind_rows(dummy_demographics) %>%
    #recode before removing duplicates per student
    dplyr::mutate(CTLEVEL = dplyr::recode(.data$DegreeLevel,
                                          `1` = 1,
                                          `2` = 2,
                                          `3` = 3,
                                          `4` = 2,
                                          `5` = 4,
                                          `6` = 7,
                                          `7` = 5,
                                          `8` = 7,
                                          `17` = 6,
                                          `18` = 6,
                                          `19` = 6,
                                          .default = 9)
           ) %>%
    dplyr::select(-.data$DegreeLevel) %>%
    #one row per student per level per unitid (keep RE/Sex/Birthdate)
    unique() %>%
    #recode and spread RaceEthnicity to get IPEDS columns
    dplyr::mutate(RaceEthnicity = recode(.data$RaceEthnicity,
                                          `1` = "CRACE17",
                                          `2` = "CRACE41",
                                          `3` = "CRACE42",
                                          `4` = "CRACE43",
                                          `5` = "CRACE44",
                                          `6` = "CRACE45",
                                          `7` = "CRACE46",
                                          `8` = "CRACE47",
                                          `9` = "CRACE23",
                                          .default = "ZRACEETH")
    ) %>%
    tidyr::spread(key = .data$RaceEthnicity, value = .data$CountRE) %>%
    #recode and spread Sex to get IPEDS columns
    dplyr::mutate(Sex = recode(.data$Sex,
                                `1` = "CRACE15",
                                `2` = "CRACE16",
                                .default = "ZRACESEX")
    ) %>%
    tidyr::spread(key = .data$Sex, value = .data$CountSex) %>%
    #recode and spread Age to get IPEDS columns
    dplyr::mutate(AgeGroup = case_when(
                                floor(.data$Age) < 18 ~ "AGE1",
                                floor(.data$Age) <= 24 ~ "AGE2",
                                floor(.data$Age) <= 39 ~ "AGE3",
                                floor(.data$Age) >= 40 ~ "AGE4",
                                is.na(.data$Age) ~ "AGE5",
                                TRUE ~ "AGE9"
                              )
    ) %>%
    tidyr::spread(key = .data$AgeGroup, value = .data$CountAge) %>%
    #aggregate and add counts in spread columns;
    #extra award levels and dummy demographics have values of 0
    dplyr::group_by(.data$Unitid, .data$CTLEVEL) %>%
    dplyr::summarize(CRACE15 = sum(.data$CRACE15, na.rm = T),
                     CRACE16 = sum(.data$CRACE16, na.rm = T),
                     CRACE17 = sum(.data$CRACE17, na.rm = T),
                     CRACE41 = sum(.data$CRACE41, na.rm = T),
                     CRACE42 = sum(.data$CRACE42, na.rm = T),
                     CRACE43 = sum(.data$CRACE43, na.rm = T),
                     CRACE44 = sum(.data$CRACE44, na.rm = T),
                     CRACE45 = sum(.data$CRACE45, na.rm = T),
                     CRACE46 = sum(.data$CRACE46, na.rm = T),
                     CRACE47 = sum(.data$CRACE47, na.rm = T),
                     CRACE23 = sum(.data$CRACE23, na.rm = T),
                     AGE1 = sum(.data$AGE1, na.rm = T),
                     AGE2 = sum(.data$AGE2, na.rm = T),
                     AGE3 = sum(.data$AGE3, na.rm = T),
                     AGE4 = sum(.data$AGE4, na.rm = T),
                     AGE5 = sum(.data$AGE5, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    #sort for easier viewing
    dplyr::arrange(.data$CTLEVEL) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=COM",
                     PART = "PART=D",
                     CTLEVEL = paste0("CTLEVEL=", .data$CTLEVEL),
                     CRACE15 = paste0("CRACE15=", .data$CRACE15),
                     CRACE16 = paste0("CRACE16=", .data$CRACE16),
                     CRACE17 = paste0("CRACE17=", .data$CRACE17),
                     CRACE41 = paste0("CRACE41=", .data$CRACE41),
                     CRACE42 = paste0("CRACE42=", .data$CRACE42),
                     CRACE43 = paste0("CRACE43=", .data$CRACE43),
                     CRACE44 = paste0("CRACE44=", .data$CRACE44),
                     CRACE45 = paste0("CRACE45=", .data$CRACE45),
                     CRACE46 = paste0("CRACE46=", .data$CRACE46),
                     CRACE47 = paste0("CRACE47=", .data$CRACE47),
                     CRACE23 = paste0("CRACE23=", .data$CRACE23),
                     AGE1 = paste0("AGE1=", .data$AGE1),
                     AGE2 = paste0("AGE2=", .data$AGE2),
                     AGE3 = paste0("AGE3=", .data$AGE3),
                     AGE4 = paste0("AGE4=", .data$AGE4),
                     AGE5 = paste0("AGE5=", .data$AGE5)
    )

  write_report(df = partD,
               component = "Completions",
               part = "PartD",
               output = output,
               format = format)

  #Error messages that would stem from recoding errors

  #Award Level
  if(("CTLEVEL=9" %in% partD$CTLEVEL) != 0) {
    svDialogs::dlg_message("Warning! Your Part D results contain unknown values for degree level.
                         Please check your data and rerun from the top.")
  }

  #RaceEthnicity
  if(("ZRACEETH" %in% colnames(partD)) != 0) {
    svDialogs::dlg_message("Warning!  Your results contain unknown values for race/ethnicity.
                         Please check your data and rerun from the top.")
  }

  #Sex
  if(("ZRACESEX" %in% colnames(partD)) != 0) {
    svDialogs::dlg_message("Warning!  Your results contain unknown values for sex.
                         Please check your data and rerun from the top.")
  }

  #Age
  if(("AGE9" %in% colnames(partD)) != 0) {
    svDialogs::dlg_message("Warning!  Your results contain unknown values for age.
                         Please check your data and rerun from the top.")
  }


}
