####
## Human Resources Uploadable for IPEDS
## Producing a key-value text file
####

## Load packages -----
library(tidyverse)

## prep datafiles -----
## set up derived columns with values
prep_hr_data_frame <- function(df) {

  df <- df %>%
        #add a combined REG column
        mutate(REG = ifelse(
          Gender == 1,
          RaceEthnicity,
          ifelse(Gender == 2,
                 RaceEthnicity + 9,
                 99)
                ),
         #set up other OccCategories based on Version 3 (complete)
         OccCategory1 = recode(OccCategory3,
                               `1` = 1,
                               `2` = 1,
                               `3` = 1,
                               `4` = 1,
                               `5` = 2,
                               `6` = 3,
                               `7` = 4,
                               `8` = 5,
                               `9` = 6,
                               `10` = 7,
                               `11` = 8,
                               `12` = 9,
                               `13` = 10,
                               `14` = 11,
                               `15` = 12,
                               `16` = 13,
                               `17` = 14,
                               `18` = 15,
                               `19` = 16,
                               `20` = 17,
                               `22` = 88, #using 88 for values that aren't used in this OccCat group
                               `23` = 88, #this way, 99 will indicate problems
                               `24` = 88,
                               .default = 99),
         OccCategory2 = recode(OccCategory3,
                               `1` = 88,
                               `2` = 88,
                               `3` = 88,
                               `4` = 88,
                               `5` = 1,
                               `6` = 2,
                               `7` = 3,
                               `8` = 3,
                               `9` = 3,
                               `10` = 3,
                               `11` = 4,
                               `12` = 5,
                               `13` = 6,
                               `14` = 7,
                               `15` = 8,
                               `16` = 9,
                               `17` = 10,
                               `18` = 11,
                               `19` = 12,
                               `20` = 13,
                               `22` = 88,
                               `23` = 88,
                               `24` = 88,
                               .default = 99),
         OccCategory4 = recode(OccCategory3,
                               `1` = 88,
                               `2` = 88,
                               `3` = 88,
                               `4` = 88,
                               `5` = 88,
                               `6` = 88,
                               `7` = 88,
                               `8` = 88,
                               `9` = 88,
                               `10` = 88,
                               `11` = 88,
                               `12` = 88,
                               `13` = 88,
                               `14` = 88,
                               `15` = 88,
                               `16` = 88,
                               `17` = 88,
                               `18` = 88,
                               `19` = 88,
                               `20` = 88,
                               `22` = 1,
                               `23` = 2,
                               `24` = 3,
                               .default = 99),
         OccCategory5 = recode(OccCategory3,
                               `1` = 1,
                               `2` = 1,
                               `3` = 1,
                               `4` = 1,
                               `5` = 2,
                               `6` = 3,
                               `7` = 4,
                               `8` = 4,
                               `9` = 4,
                               `10` = 4,
                               `11` = 5,
                               `12` = 6,
                               `13` = 7,
                               `14` = 8,
                               `15` = 9,
                               `16` = 10,
                               `17` = 11,
                               `18` = 12,
                               `19` = 13,
                               `20` = 14,
                               `22` = 88,
                               `23` = 88,
                               `24` = 88,
                               .default = 99),
         #set up instructional function based on occcat3
         InstFunction = recode(OccCategory3,
                               `1` = 1,
                               `2` = 2,
                               `3` = 3,
                               `4` = 5,
                               `5` = 88,
                               `6` = 88,
                               `7` = 88,
                               `8` = 88,
                               `9` = 88,
                               `10` = 88,
                               `11` = 88,
                               `12` = 88,
                               `13` = 88,
                               `14` = 88,
                               `15` = 88,
                               `16` = 88,
                               `17` = 88,
                               `18` = 88,
                               `19` = 88,
                               `20` = 88,
                               `22` = 88,
                               `23` = 88,
                               `24` = 88,
                               .default = 99),
         #set up an instructional flag based on Occcat3
         Instructional = recode(OccCategory3,
                                `1` = 1,
                                `2` = 1,
                                `3` = 1,
                                `4` = 1,
                                `5` = 0,
                                `6` = 0,
                                `7` = 0,
                                `8` = 0,
                                `9` = 0,
                                `10` = 0,
                                `11` = 0,
                                `12` = 0,
                                `13` = 0,
                                `14` = 0,
                                `15` = 0,
                                `16` = 0,
                                `17` = 0,
                                `18` = 0,
                                `19` = 0,
                                `20` = 0,
                                `22` = 0,
                                `23` = 0,
                                `24` = 0,
                                .default = 99),
         #set up default: 1 row = Count 1 so we can add later
         Count = 1,
         #specify EmpId as character to avoid type problems
         EmpId = as.character(EmpId))

  return(df)
}


## Warnings from recoding failures  -----

#RaceEthnicityGender
if(99 %in% ipeds_df$REG) {
  svDialogs::dlg_message("Warning! Some RaceEthnicityGender combinations have failed.
                         Please check your Gender and RaceEthnicity values, then rerun from the top.")
}

#OccCats
if(99 %in% c(ipeds_df$OccCategory1, ipeds_df$OccCategory2, ipeds_df$OccCategory4, ipeds_df$OccCategory5)){
  svDialogs::dlg_message("Warning!  Some Occupational Category recoding has failed.
                         Please check your OccCategory3 values, then rerun from the top.")
}

#Months (for Salary-spread); only matters for current employees
if(sum(!(ipeds_df$Months[ipeds_df$CurrentEmployee == 1] %in% c(8, 9, 10, 11, 12, 99))) != 0){
  svDialogs::dlg_message("Warning!  Some Months values are not allowed and will break the Salary calculation in G1.
                         Please check your data to ensure use of 8, 9, 10, 11, 12, and 99 only, then rerun from the top.")
}

## Function definitions -----

## Part A1 --- Count of FT Instructional staff by tenure status, academic rank, and race/ethnicity/gender
make_hr_part_A1 <- function(df) {

  #set up the grid of options
  combos_A1 <- expand.grid(Unitid = ipeds_unitid,
                           Tenure = c(1:7),
                           Rank = c(1:6),
                           REG = c(1:18),
                           Count = 0) %>%
               bind_rows(expand.grid(Unitid = ipeds_unitid,
                                     Tenure = 6,
                                     Rank = 7,
                                     REG = 1:18,
                                     Count = 0)
                         )

  #produce the uploadable format
  partA1 <- df %>%
            filter(CurrentEmployee == 1,
                   Instructional == 1,
                   FtPt == 'F') %>%
            select(Unitid, Tenure, Rank, REG, Count) %>%
            #add extra combinations
            bind_rows(combos_A1) %>%
            #aggregate the full data
            group_by(Unitid, Tenure, Rank, REG) %>%
            summarize(Count = sum(Count)) %>% ungroup() %>%
            #sort for easy viewing
            arrange(Tenure, Rank, REG) %>%
            #format for upload
            transmute(UNITID = paste0("UNITID=", Unitid),
                      SURVSECT = "SURVSECT=HR1",
                      PART = "PART=A1",
                      TENURE = paste0("TENURE=", Tenure),
                      RANK = paste0("RANK=", Rank),
                      RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", REG),
                      COUNT = paste0("COUNT=", Count))

  #just this part
  write.table(x = partA1, sep = ",",
              file = paste0(path, "HumanResources_PartA1_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  #the upload doc
  write.table(x = partA1, sep = ",",
              file = paste0(path, "HumanResources_PartsAll_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)
}

## Part A2 --- Count of FT instructional staff by tenure status, medical school, and function
make_hr_part_A2 <- function(df) {

  #set up the grid of options
  combos_A2 <- expand.grid(Unitid = ipeds_unitid,
                           Tenure = c(1:7),
                           IsMedical = c(0:1),
                           InstFunction = c(1:3, 5),  #4 is generated by the system (subtotal of 1:3)
                           Count = 0)

  #produce the uploadable format
  partA2 <- df %>%
            filter(CurrentEmployee == 1,
                   Instructional == 1, #instructional
                   FtPt == 'F') %>%
            select(Unitid, Tenure, IsMedical, InstFunction, Count) %>%
            #add extra combinations
            bind_rows(combos_A2) %>%
            #aggregate the full data
            group_by(Unitid, Tenure, IsMedical, InstFunction) %>%
            summarize(Count = sum(Count)) %>%
            ungroup() %>%
            #sort for easy viewing
            arrange(Tenure, IsMedical, InstFunction) %>%
            #format for upload
            transmute(UNITID = paste0("UNITID=", Unitid),
                   SURVSECT = "SURVSECT=HR1",
                   PART = "PART=A2",
                   TENURE = paste0("TENURE=", Tenure),
                   ISMEDICAL = paste0("ISMEDICAL=", IsMedical),
                   INSTFUNCTION = paste0("INSTFUNCTION=", InstFunction),
                   COUNT = paste0("COUNT=", Count))


  #just this part
  write.table(x = partA2, sep = ",",
              file = paste0(path, "HumanResources_PartA2_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  #the upload doc
  write.table(x = partA2, sep = ",",
              file = paste0(path, "HumanResources_PartsAll_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
}

## Part B1 --- Count of FT Non-instructional staff by occupational category
make_hr_part_B1 <- function(df) {

  #set up the grid of options
  combos_B1 <- expand.grid(Unitid = ipeds_unitid,
                           OccCategory1 = c(1:17),
                           REG = c(1:18),
                           Count = 0)

  #produce the uploadable format
  partB1 <- df %>%
            filter(CurrentEmployee == 1,
                   Instructional == 0, #non-instructional
                   FtPt == 'F') %>%
            select(Unitid, OccCategory1, REG, Count) %>%
            #add extra combinations
            bind_rows(combos_B1) %>%
            #aggregate the full data
            group_by(Unitid, OccCategory1, REG) %>%
            summarize(Count = sum(Count)) %>%
            ungroup() %>%
            #sort for easy viewing
            arrange(OccCategory1, REG) %>%
            #format for upload
            transmute(UNITID = paste0("UNITID=", Unitid),
                      SURVSECT = "SURVSECT=HR1",
                      PART = "PART=B1",
                      OCCCATEGORY1 = paste0("OCCCATEGORY1=", OccCategory1),
                      RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", REG),
                      COUNT = paste0("COUNT=", Count))

  #just this part
  write.table(x = partB1, sep = ",",
              file = paste0(path, "HumanResources_PartB1_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  #the upload doc
  write.table(x = partB1, sep = ",",
              file = paste0(path, "HumanResources_PartsAll_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
}

## Part B2 --- Full-time non-instructional staff by tenure, medical school, and occupational category
make_hr_part_B2 <- function(df) {

  #set up the grid of options
  combos_B2 <- expand.grid(Unitid = ipeds_unitid,
                           Tenure = c(1:7),
                           IsMedical = c(0:1),
                           OccCategory1 = c(2:12),
                           Count = 0)

  #produce the uploadable format
  partB2 <- df %>%
            filter(CurrentEmployee == 1,
                   Instructional == 0,
                   FtPt == 'F',
                   OccCategory1 %in% c(2:12)) %>%
            select(Unitid, Tenure, IsMedical, OccCategory1, Count) %>%
            #add extra combinations
            bind_rows(combos_B2) %>%
            #aggregate the full data
            group_by(Unitid, Tenure, IsMedical, OccCategory1) %>%
            summarize(Count = sum(Count)) %>%
            ungroup() %>%
            #sort for easy viewing
            arrange(Tenure, IsMedical, OccCategory1) %>%
            #format for upload
            transmute(UNITID = paste0("UNITID=", Unitid),
                      SURVSECT = "SURVSECT=HR1",
                      PART = "PART=B2",
                      TENURE = paste0("TENURE=", Tenure),
                      ISMEDICAL = paste0("ISMEDICAL=", IsMedical),
                      OCCCATEGORY1 = paste0("OCCCATEGORY1=", OccCategory1),
                      COUNT = paste0("COUNT=", Count))

  #just this part
  write.table(x = partB2, sep = ",",
              file = paste0(path, "HumanResources_PartB2_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  #the upload doc
  write.table(x = partB2, sep = ",",
              file = paste0(path, "HumanResources_PartsAll_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
}

## Part B3 --- Full-time non-instructional staff by medical school, and occupational category
make_hr_part_B3 <- function(df) {

  #set up the grid of options
  combos_B3 <- expand.grid(Unitid = ipeds_unitid,
                           IsMedical = c(0:1),
                           OccCategory1 = c(13:17),
                           Count = 0)

  #produce the uploadable format
  partB3 <- df %>%
            filter(CurrentEmployee == 1,
                   Instructional == 0,
                   FtPt == 'F',
                   OccCategory1 %in% c(13:17)) %>%
            select(Unitid, IsMedical, OccCategory1, Count) %>%
            #add extra combinations
            bind_rows(combos_B3) %>%
            #aggregate the full data
            group_by(Unitid, IsMedical, OccCategory1) %>%
            summarize(Count = sum(Count)) %>%
            ungroup() %>%
            #sort for easy viewing
            arrange(IsMedical, OccCategory1) %>%
            #format for upload
            transmute(UNITID = paste0("UNITID=", Unitid),
                      SURVSECT = "SURVSECT=HR1",
                      PART = "PART=B3",
                      ISMEDICAL = paste0("ISMEDICAL=", IsMedical),
                      OCCCATEGORY1 = paste0("OCCCATEGORY1=", OccCategory1),
                      COUNT = paste0("COUNT=", Count))

  #just this part
  write.table(x = partB3, sep = ",",
              file = paste0(path, "HumanResources_PartB3_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  #the upload doc
  write.table(x = partB3, sep = ",",
              file = paste0(path, "HumanResources_PartsAll_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
}

## Part D1 --- Part-time staff by occupational category
make_hr_part_D1 <- function(df) {

  #set up the grid of options
  combos_D1 <- expand.grid(Unitid = ipeds_unitid,
                           OccCategory1 = c(1:18),
                           REG = c(1:18),
                           Count = 0)

  #produce the uploadable format
  partD1 <- df %>%
            filter(CurrentEmployee == 1,
                   FtPt == 'P',
                   OccCategory1 %in% c(1:18)) %>%
            select(Unitid, OccCategory1, REG, Count) %>%
            #add extra combinations
            bind_rows(combos_D1) %>%
            #aggregate the full data
            group_by(Unitid, OccCategory1, REG) %>%
            summarize(Count = sum(Count)) %>%
            ungroup() %>%
            #sort for easy viewing
            arrange(OccCategory1, REG) %>%
            #format for upload
            transmute(UNITID = paste0("UNITID=", Unitid),
                      SURVSECT = "SURVSECT=HR1",
                      PART = "PART=D1",
                      OCCCATEGORY1 = paste0("OCCCATEGORY1=", OccCategory1),
                      RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", REG),
                      COUNT = paste0("COUNT=", Count))

  #just this part
  write.table(x = partD1, sep = ",",
              file = paste0(path, "HumanResources_PartD1_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  #the upload doc
  write.table(x = partD1, sep = ",",
              file = paste0(path, "HumanResources_PartsAll_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
}

## Part D2 --- Graduate assistants by occupational category and race/ethnicity/gender
make_hr_part_D2 <- function(df) {

  #set up the grid of options
  combos_D2 <- expand.grid(Unitid = ipeds_unitid,
                           OccCategory4 = c(1:3),
                           REG = c(1:18),
                           Count = 0)

  #produce the uploadable format
  partD2 <- df %>%
            filter(CurrentEmployee == 1,
                   OccCategory4 %in% c(1:3)) %>%
            select(Unitid, OccCategory4, REG, Count) %>%
            #add extra combinations
            bind_rows(combos_D2) %>%
            #aggregate the full data
            group_by(Unitid, OccCategory4, REG) %>%
            summarize(Count = sum(Count)) %>%
            ungroup() %>%
            #sort for easy viewing
            arrange(OccCategory4, REG) %>%
            #format for upload
            transmute(UNITID = paste0("UNITID=", Unitid),
                      SURVSECT = "SURVSECT=HR1",
                      PART = "PART=D2",
                      OCCCATEGORY4 = paste0("OCCCATEGORY4=", OccCategory4),
                      RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", REG),
                      COUNT = paste0("COUNT=", Count))

  #just this part
  write.table(x = partD2, sep = ",",
              file = paste0(path, "HumanResources_PartD2_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  #the upload doc
  write.table(x = partD2, sep = ",",
              file = paste0(path, "HumanResources_PartsAll_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
}

## Part D3 --- Part-time staff by tenure, medical school, and occupational category
make_hr_part_D3 <- function(df) {

  #set up the grid of options
  combos_D3 <- expand.grid(Unitid = ipeds_unitid,
                           Tenure = c(1:7),
                           IsMedical = c(0:1),
                           OccCategory3 = c(1:15),
                           Count = 0)

  #produce the uploadable format
  partD3 <- df %>%
            filter(CurrentEmployee == 1,
                   FtPt == "P",
                   OccCategory3 %in% c(1:15)) %>%
            select(Unitid, Tenure, IsMedical, OccCategory3, Count) %>%
            #add extra combinations
            bind_rows(combos_D3) %>%
            #aggregate the full data
            group_by(Unitid, Tenure, IsMedical, OccCategory3) %>%
            summarize(Count = sum(Count)) %>%
            ungroup() %>%
            #sort for easy viewing
            arrange(Tenure, IsMedical, OccCategory3) %>%
            #format for upload
            transmute(UNITID = paste0("UNITID=", Unitid),
                      SURVSECT = "SURVSECT=HR1",
                      PART = "PART=D3",
                      TENURE = paste0("TENURE=", Tenure),
                      ISMEDICAL = paste0("ISMEDICAL=", IsMedical),
                      OCCCATEGORY3 = paste0("OCCCATEGORY3=", OccCategory3),
                      COUNT = paste0("COUNT=", Count))

  #just this part
  write.table(x = partD3, sep = ",",
              file = paste0(path, "HumanResources_PartD3_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  #the upload doc
  write.table(x = partD3, sep = ",",
              file = paste0(path, "HumanResources_PartsAll_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
}

## Part D4 --- Part-time Non-instructional staff by medical school, and occupational category
make_hr_part_D4 <- function(df) {

  #set up the grid of options
  combos_D4 <- expand.grid(Unitid = ipeds_unitid,
                           IsMedical = c(0:1),
                           OccCategory3 = c(16:20, 22:24),
                           Count = 0)

  #produce the uploadable format
  partD4 <- df %>%
            filter(CurrentEmployee == 1,
                   FtPt == "P",
                   OccCategory3 %in% c(16:20, 22:24)) %>%
            select(Unitid, IsMedical, OccCategory3, Count) %>%
            #add extra combinations
            bind_rows(combos_D4) %>%
            #aggregate the full data
            group_by(Unitid, IsMedical, OccCategory3) %>%
            summarize(Count = sum(Count)) %>%
            ungroup() %>%
            #sort for easy viewing
            arrange(IsMedical, OccCategory3) %>%
            #format for upload
            transmute(UNITID = paste0("UNITID=", Unitid),
                      SURVSECT = "SURVSECT=HR1",
                      PART = "PART=D4",
                      ISMEDICAL = paste0("ISMEDICAL=", IsMedical),
                      OCCCATEGORY3 = paste0("OCCCATEGORY3=", OccCategory3),
                      COUNT = paste0("COUNT=", Count))

  #just this part
  write.table(x = partD4, sep = ",",
              file = paste0(path, "HumanResources_PartD4_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  #the upload doc
  write.table(x = partD4, sep = ",",
              file = paste0(path, "HumanResources_PartsAll_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
}

## Part G1 --- Salaries of Instructional staff
make_hr_part_G1 <- function(df) {

  #set up the grid of options
  combos_G1 <- expand.grid(Unitid = ipeds_unitid,
                           Rank = c(1:6),
                           Gender = c(1:2),
                           Months = c(8:12),
                           Salary = 0,
                           EmpId = 'dummy_id',
                           Count = 0)

  #produce the uploadable format
  partG1 <- df %>%
            filter(CurrentEmployee == 1,
                   Instructional == 1, #instructional
                   FtPt == "F") %>%  #not listed in uploadable instructions -- verify?
            select(Unitid, Rank, Gender, Months, Salary, EmpId, Count) %>%
            #add extra combinations
            bind_rows(combos_G1) %>%
            #reset rank 7 as rank 6
            mutate(Rank = ifelse(Rank == 7, 6, Rank),
                   months_count = recode(Months,
                                         `12` = '12mCount',
                                         `11` = '11mCount',
                                         `10` = '10mCount',
                                         `9` = '9mCount',
                                         `8` = 'LessThan9mCount',
                                         .default = 'ZzCount'),
                   salary_sum = recode(Months,
                                       `12` = '12mSoutlays',
                                       `11` = '11mSoutlays',
                                       `10` = '10mSoutlays',
                                       `9` = '9mSoutlays',
                                       .default = 'ZzSoutlays')
                   ) %>%
            spread(key = months_count, value = Count) %>%
            spread(key = salary_sum, value = Salary) %>%
            #aggregate the full data
            group_by(Unitid, Rank, Gender) %>%
            summarize(`12mCount` = sum(`12mCount`, na.rm = T),
                      `11mCount` = sum(`11mCount`, na.rm = T),
                      `10mCount` = sum(`10mCount`, na.rm = T),
                      `9mCount` = sum(`9mCount`, na.rm = T),
                      `LessThan9mCount` = sum(`LessThan9mCount`, na.rm = T),
                      #`ZzCount` = sum(`ZzCount`, na.rm = T),
                      `12mSoutlays` = sum(`12mSoutlays`, na.rm = T),
                      `11mSoutlays` = sum(`11mSoutlays`, na.rm = T),
                      `10mSoutlays` = sum(`10mSoutlays`, na.rm = T),
                      `9mSoutlays` = sum(`9mSoutlays`, na.rm = T),
                      `ZzSoutlays` = sum(`ZzSoutlays`, na.rm = T) #this will have values for the LessThan9 folks
            ) %>%
            ungroup() %>%
            #sort for easy viewing
            arrange(Rank,Gender) %>%
            #format for upload
            transmute(UNITID = paste0("UNITID=", Unitid),
                      SURVSECT = "SURVSECT=HR1",
                      PART = "PART=G1",
                      RANK = paste0("RANK=", Rank),
                      GENDER = paste0("GENDER=", Gender),
                      `12mCount` = paste0("12MCOUNT=", `12mCount`),
                      `11mCount` = paste0("11MCOUNT=", `11mCount`),
                      `10mCount` = paste0("10MCOUNT=", `10mCount`),
                      `9mCount` = paste0("9MCOUNT=", `9mCount`),
                      `LessThan9mCount` = paste0("LESSTHAN9MCOUNT=", LessThan9mCount),
                      #`ZzCount` = paste0("ZzCount=", ZzCount),
                      `12mSoutlays` = paste0("12MSOUTLAYS=", `12mSoutlays`),
                      `11mSoutlays` = paste0("11MSOUTLAYS=", `11mSoutlays`),
                      `10mSoutlays` = paste0("10MSOUTLAYS=", `10mSoutlays`),
                      `9mSoutlays` = paste0("9MSOUTLAYS=", `9mSoutlays`)#,
                      #`ZzSoutlays` = paste0("ZzSoutlays=", ZzSoutlays)
                      )

  #just this part
  write.table(x = partG1, sep = ",",
              file = paste0(path, "HumanResources_PartG1_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  #the upload doc
  write.table(x = partG1, sep = ",",
              file = paste0(path, "HumanResources_PartsAll_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
}

## Part G2 --- Salaries of non-instructional staff
make_hr_part_G2 <- function(df) {

  #set up the grid of options
  combos_G2 <- expand.grid(Unitid = ipeds_unitid,
                           OccCategory2 = c(1:13),
                           Salary = 0,
                           EmpId = 'dummy_id',
                           Count = 0)

  #produce the uploadable format
  partG2 <- df %>%
            filter(CurrentEmployee == 1,
                   Instructional == 0, #non-instructional
                   FtPt == "F") %>%  #not listed in uploadable instructions -- verify?
            select(Unitid, OccCategory2, Salary, EmpId, Count) %>%
            #add extra combinations
            bind_rows(combos_G2) %>%
            #aggregate the full data
            group_by(Unitid, OccCategory2) %>%
            summarize(Salary = sum(Salary)) %>%
            ungroup() %>%
            #sort for easy viewing
            arrange(OccCategory2) %>%
            #format for upload
            transmute(UNITID = paste0("UNITID=", Unitid),
                      SURVSECT = "SURVSECT=HR1",
                      PART = "PART=G2",
                      OCCCATEGORY2 = paste0("OCCCATEGORY2=", OccCategory2),
                      SOUTLAYS = paste0("SOUTLAYS=", Salary))

  #just this part
  write.table(x = partG2, sep = ",",
              file = paste0(path, "HumanResources_PartG2_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  #the upload doc
  write.table(x = partG2, sep = ",",
              file = paste0(path, "HumanResources_PartsAll_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
}

## Part H1 --- Full-time new hire instructional staff by tenure status and race/ethnicity/gender
make_hr_part_H1 <- function(df) {

  #set up the grid of options
  combos_H1 <- expand.grid(Unitid = ipeds_unitid,
                           Tenure = c(1:7),
                           REG = c(1:18),
                           Count = 0)

  #produce the uploadable format
  partH1 <- df %>%
            filter(Instructional == 1,
                   NewHire == 1,
                   FtPt == 'F') %>%
            select(Unitid, Tenure, REG, Count) %>%
            #add extra combinations
            bind_rows(combos_H1) %>%
            #aggregate the full data
            group_by(Unitid, Tenure, REG) %>%
            summarize(Count = sum(Count)) %>%
            ungroup() %>%
            #sort for easy viewing
            arrange(Tenure, REG) %>%
            #format for upload
            transmute(UNITID = paste0("UNITID=", Unitid),
                      SURVSECT = "SURVSECT=HR1",
                      PART = "PART=H1",
                      TENURE = paste0("TENURE=", Tenure),
                      RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", REG),
                      COUNT = paste0("COUNT=", Count))

  #just this part
  write.table(x = partH1, sep = ",",
              file = paste0(path, "HumanResources_PartH1_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  #the upload doc
  write.table(x = partH1, sep = ",",
              file = paste0(path, "HumanResources_PartsAll_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
}

## Part H2 --- New hires by occupational category, Race/Ethnicity/Gender
make_hr_part_H2 <- function(df) {

  #set up the grid of options
  combos_H2 <- expand.grid(Unitid = ipeds_unitid,
                           OccCategory5 = c(2:14),
                           REG = c(1:18),
                           Count = 0)

  #produce the uploadable format
  partH2 <- df %>%
            filter(NewHire == 1,
                   FtPt == 'F',
                   OccCategory5 %in% c(2:14)) %>%
            select(Unitid, OccCategory5, REG, Count) %>%
            #add extra combinations
            bind_rows(combos_H2) %>%
            #aggregate the full data
            group_by(Unitid, OccCategory5, REG) %>%
            summarize(Count = sum(Count)) %>%
            ungroup() %>%
            #sort for easy viewing
            arrange(OccCategory5, REG) %>%
            #format for upload
            transmute(UNITID = paste0("UNITID=", Unitid),
                      SURVSECT = "SURVSECT=HR1",
                      PART = "PART=H2",
                      OCCCATEGORY5 = paste0("OCCCATEGORY5=", OccCategory5),
                      RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", REG),
                      COUNT = paste0("COUNT=", Count))

  #just this part
  write.table(x = partH2, sep = ",",
              file = paste0(path, "HumanResources_PartH2_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  #the upload doc
  write.table(x = partH2, sep = ",",
              file = paste0(path, "HumanResources_PartsAll_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
}

#################
## Status message: finished

svDialogs::dlg_message(paste0("Human Resources file available. Please see results at ", path))




