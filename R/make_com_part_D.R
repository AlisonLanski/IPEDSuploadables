#' Make Completions Part D
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
make_com_part_D <- function(df) {

  #check extracips list for award levels not included in the startingdf
  extralevel_D <- extracips %>%
    select(Unitid, DegreeLevel) %>%
    unique() %>%
    filter(!(DegreeLevel %in% df$DegreeLevel)) %>%
    #add dummy data to any award levels found
    mutate(StudentId = 'dummy_studentid',
           RaceEthnicity = 1,
           Sex = 1,
           Birthdate = lubridate::ymd("1900-01-01"),
           CountRE = 0,
           CountSex = 0,
           CountAge = 0
    ) %>%
    #reorder for rbind
    select(Unitid, StudentId, everything())

  #set up an df with 0-rows to ensure we get all
  #race/ethnicity, sex, and age categories in the final output
  dummy_demographics <- data.frame(Unitid = ipeds_unitid,
                                   StudentId = 'dummy_studentid',
                                   DegreeLevel = max(df$DegreeLevel),
                                   RaceEthnicity = c(1:9),
                                   Sex = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
                                   Age = c(15, 20, 25, 30, 35, 40, 45, 50, NA),
                                   CountRE = 0,
                                   CountSex = 0,
                                   CountAge = 0,
                                   stringsAsFactors = FALSE)

  partD <- df %>%
    select(Unitid, StudentId, DegreeLevel, RaceEthnicity, Sex, Age) %>%
    #add values which will be summed later
    mutate(CountRE = 1,
           CountSex = 1,
           CountAge = 1
    ) %>%
    #add any extra award levels
    bind_rows(extralevel_D) %>%
    #add dummy demographics to make sure the spread works correctly later
    bind_rows(dummy_demographics) %>%
    #recode before removing duplicates per student
    mutate(CTLEVEL = recode(DegreeLevel,
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
                            .default = 9)) %>%
    select(-DegreeLevel) %>%
    #one row per student per level per unitid (keep RE/Sex/Birthdate)
    unique() %>%
    #recode and spread RaceEthnicity to get IPEDS columns
    mutate(RaceEthnicity = recode(RaceEthnicity,
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
    spread(key = RaceEthnicity, value = CountRE) %>%
    #recode and spread Sex to get IPEDS columns
    mutate(Sex = recode(Sex,
                        `1` = "CRACE15",
                        `2` = "CRACE16",
                        .default = "ZRACESEX")
    ) %>%
    spread(key = Sex, value = CountSex) %>%
    #recode and spread Age to get IPEDS columns
    mutate(AgeGroup = case_when(
      floor(Age) < 18 ~"AGE1",
      floor(Age) <= 24 ~ "AGE2",
      floor(Age) <= 39 ~ "AGE3",
      floor(Age) >= 40 ~ "AGE4",
      is.na(Age) ~ "AGE5",
      TRUE ~ "AGE9"
    )
    ) %>%
    spread(key = AgeGroup, value = CountAge) %>%
    #aggregate and add counts in spread columns;
    #extra award levels and dummy demographics have values of 0
    group_by(Unitid, CTLEVEL) %>%
    summarize(CRACE15 = sum(CRACE15, na.rm = T),
              CRACE16 = sum(CRACE16, na.rm = T),
              CRACE17 = sum(CRACE17, na.rm = T),
              CRACE41 = sum(CRACE41, na.rm = T),
              CRACE42 = sum(CRACE42, na.rm = T),
              CRACE43 = sum(CRACE43, na.rm = T),
              CRACE44 = sum(CRACE44, na.rm = T),
              CRACE45 = sum(CRACE45, na.rm = T),
              CRACE46 = sum(CRACE46, na.rm = T),
              CRACE47 = sum(CRACE47, na.rm = T),
              CRACE23 = sum(CRACE23, na.rm = T),
              AGE1 = sum(AGE1, na.rm = T),
              AGE2 = sum(AGE2, na.rm = T),
              AGE3 = sum(AGE3, na.rm = T),
              AGE4 = sum(AGE4, na.rm = T),
              AGE5 = sum(AGE5, na.rm = T)
    ) %>%
    ungroup() %>%
    #sort for easier viewing
    arrange(CTLEVEL) %>%
    #format for upload
    mutate(UNITID = paste0("UNITID=", Unitid),
           SURVSECT = "SURVSECT=COM",
           PART = "PART=D",
           CTLEVEL = paste0("CTLEVEL=", CTLEVEL),
           CRACE15 = paste0("CRACE15=", CRACE15),
           CRACE16 = paste0("CRACE16=", CRACE16),
           CRACE17 = paste0("CRACE17=", CRACE17),
           CRACE41 = paste0("CRACE41=", CRACE41),
           CRACE42 = paste0("CRACE42=", CRACE42),
           CRACE43 = paste0("CRACE43=", CRACE43),
           CRACE44 = paste0("CRACE44=", CRACE44),
           CRACE45 = paste0("CRACE45=", CRACE45),
           CRACE46 = paste0("CRACE46=", CRACE46),
           CRACE47 = paste0("CRACE47=", CRACE47),
           CRACE23 = paste0("CRACE23=", CRACE23),
           AGE1 = paste0("AGE1=", AGE1),
           AGE2 = paste0("AGE2=", AGE2),
           AGE3 = paste0("AGE3=", AGE3),
           AGE4 = paste0("AGE4=", AGE4),
           AGE5 = paste0("AGE5=", AGE5)
    ) %>%
    select(UNITID, SURVSECT, PART, CTLEVEL,
           CRACE15, CRACE16, CRACE17, CRACE41,
           CRACE42, CRACE43, CRACE44, CRACE45,
           CRACE46, CRACE47, CRACE23, AGE1,
           AGE2, AGE3, AGE4, AGE5)

  #just this part
  write.table(x = partD, sep = ",",
              file = paste0(path, "Completions_PartD_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  #append to the upload doc
  write.table(x = partD, sep = ",",
              file = paste0(path, "Completions_PartsAll_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

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
