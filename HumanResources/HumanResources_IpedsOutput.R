#########################################################
#### 
## Human Resources Uploadable for IPEDS
## Producing a key-value text file
####
#########################################################

## UNLV data manip -----
## Exchange\FACULTY\Fall 2018\UNLV Faculty Salaries 2019
## Exchange\IPEDS\2018-19\Supporting Docs\Human Resources\2017 All with Addl Jobs Removed.xlsx
## Exchange\FACULTY\Fall 2018\Workday report extracts\Workday IPEDS report extracts\IPEDS G...xlsx
library(readxl)

gdf <- read_excel("/Volumes/Staff/Groups/Decision Support/Exchange/FACULTY/Fall 2018/Workday report extracts/Workday IPEDS report extracts/IPEDS G - Salary Worksheet and Salary Outlays for Instructional Staff - HCM - CR (NSHE).xlsx") %>%
       select(`Employee ID`, `Total Base Pay Annualized - Amount`, `Annual Work Period`)
ipeds_df <- read_excel("/Volumes/Staff/Groups/Decision Support/Exchange/Shiloh/IPEDS-Uploadables/HumanResources/FALL 2018 COMBINED IPEDS DATA SET.xlsx") %>%
            left_join(gdf, by = "Employee ID") %>%
            transmute(EmpId = NSHE_ID,
                      RaceEthnicity = case_when(
                              `IPEDS Ethnicity Evaluated` == "Nonresident Alien" ~ 1,
                              `IPEDS Ethnicity Evaluated` == "Hispanic" ~ 2,
                              `IPEDS Ethnicity Evaluated` == "Native American" ~ 3,
                              `IPEDS Ethnicity Evaluated` == "Asian" ~ 4,
                              `IPEDS Ethnicity Evaluated` == "African American" ~ 5,
                              `IPEDS Ethnicity Evaluated` == "Native Hawaiian or Pacific Islander" ~ 6,
                              `IPEDS Ethnicity Evaluated` == "White" ~ 7,
                              `IPEDS Ethnicity Evaluated` == "Two or More" ~ 8,
                              `IPEDS Ethnicity Evaluated` == "unknown" ~ 9
                            ),
                      Gender = case_when(
                              str_detect(Gender, "Male") ~ 1,
                              str_detect(Gender, "Female") ~ 2
                            ),
                      Rank = case_when(
                              `Academic Rank` == "Professor" ~ 1,
                              `Academic Rank` == "Associate" ~ 2,
                              `Academic Rank` == "Assistant" ~ 3,
                              `Academic Rank` == "Instructor" ~ 4,
                              `Academic Rank` == "Lecturer" ~ 5,
                              `Academic Rank` == "No Rank" ~ 6,
                              `Academic Rank` == "-" ~ 7
                            ),
                      Tenure = case_when(
                              `Tenure Status` == "Tenured" ~ 1,       
                              `Tenure Status` == "On Tenure - Track" ~ 2,
                              `Tenure Status` == "Non Tenure - Track" ~ 4
                            ),
                      IsMedical = case_when(
                              `IPEDS Medical / Non-Medical Staff` == "Medical School Staff" ~ 1,
                              `IPEDS Medical / Non-Medical Staff` == "Non-Medical School Staff" ~ 0
                            ) %>%
                            replace_na(0),
                      NewHire = if_else(str_extract(`Hire Date`, "\\d{4}") == "2019", 1, 0) %>%
                               replace_na(0),
                      FtPt = if_else(FTE == 1, "F", "P"),
                      Salary = `Total Base Pay Annualized - Amount` %>%
                                replace_na(0),
                      Months = str_extract(`Annual Work Period`, "\\d{1,2}") %>%
                               replace_na(99) %>%
                               as.numeric(),
                      OccCategory3 = case_when(
                               `Job Family` == "Instruction" ~ 1,
                               # str_detect(`Job Classification Mapping`, "") ~ 2,
                               # str_detect(`Job Classification Mapping`, "") ~ 3,
                               # str_detect(`Job Classification Mapping`, "") ~ 4,
                               # str_detect(`Job Classification Mapping`, "") ~ 5,
                               str_detect(`Job Classification Mapping`, "") ~ 6,
                               str_detect(`Job Classification Mapping`, "25\\-4010") ~ 7,
                               str_detect(`Job Classification Mapping`, "25\\-4020") ~ 8,
                               str_detect(`Job Classification Mapping`, "25\\-4030") ~ 9,
                               str_detect(`Job Classification Mapping`, "25\\-[239]0{3}") ~ 10,
                               str_detect(`Job Classification Mapping`, "11\\-0{4}") ~ 11,
                               str_detect(`Job Classification Mapping`, "13\\-0{4}") ~ 12,
                               str_detect(`Job Classification Mapping`, "1[579]\\-0{4}") ~ 13,
                               str_detect(`Job Classification Mapping`, "2[137]\\-0{4}") ~ 14,
                               str_detect(`Job Classification Mapping`, "29\\-0{4}") ~ 15,
                               str_detect(`Job Classification Mapping`, "3[13579]\\-0{4}") ~ 16,
                               str_detect(`Job Classification Mapping`, "41\\-0{4}") ~ 17,
                               str_detect(`Job Classification Mapping`, "43\\-0{4}") ~ 18,
                               str_detect(`Job Classification Mapping`, "4[579]\\-0{4}") ~ 19,
                               str_detect(`Job Classification Mapping`, "5[13]-0{4}") ~ 20,
                               str_detect(`Job Classification Mapping`, "25\\-9044") ~ 22#,
                               # str_detect(`Job Classification Mapping`, "") ~ 23,
                               # str_detect(`Job Classification Mapping`, "") ~ 24
                            ) %>%
                       replace_na(15) ## For some reason, one medical professional is listed as NA
                      )

#########################################################
###
## Set up variables -----

#load package
library(tidyverse)


#set an output path:
path <- svDialogs::dlg_dir(default = getwd(), title = "Select the file output location")$res 

#make sure the final / is in the path (before the filename)
if(!str_detect(path, pattern = "/$")) {
  path <- paste0(path, "/")
}


#set the school's unitid (for later)
ipeds_unitid <- svDialogs::dlgInput("What is your school's IPEDS Unitid?")$res


#set a dummy employeeID  (for later)
dummy_id <- svDialogs::dlgInput("Provide a value that can be used as a dummy-employee ID")$res



#prep datafiles: set up derived columns with values -----

ipeds_df <- ipeds_df %>%
  mutate(Unitid = ipeds_unitid, ## 
         #add a combined REG column
         REG = ifelse(Gender == 1,
                      RaceEthnicity,
                      ifelse(Gender == 2,
                             RaceEthnicity+9,
                             99)),
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
                               `4` = 4,
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
         Count = 1)


#eda comparison
#ipeds_df %>% count(OccCategory3, OccCategory1, OccCategory2, OccCategory4, OccCategory5, InstFunction) %>% View()


############
## Warnings from recoding failures

#RaceEthnicityGender
if(99 %in% ipeds_df$REG) {
  svDialogs::dlg_message("Warning! Some RaceEthnicityGender combinations have failed. 
                         Please check your Gender and RaceEthnicity values, then rerun from the top.")
}

#OccCats 
if(99 %in% c(ipeds_df$OccCategory1, ipeds_df$OccCategory2, ipeds_df$OccCategory4)){
  svDialogs::dlg_message("Warning!  Some Occupational Category recoding has failed.  
                         Please check your OccCategory3 values, then rerun from the top.")
}


#Months (for Salary-spread)
if(sum(!(ipeds_df$Months %in% c(8, 9, 10, 11, 12, 99))) != 0){
  svDialogs::dlg_message("Warning!  Some Months values are not allowed and will break the Salary calculation in G1.
                         Please check your data to ensure use of 8, 9, 10, 11, 12, and 99 only, then rerun from the top.")
}




#####################################################################
####
##    Produce upload files


###
### NOTE 
### On the use of sorting (arrange) in each step below
###
### IPEDS require each part to be in order and specific sorting within each part
##  Putting the parts in order is done by appending one a time to output file
##  Sorting within each part is done by arrange within the data preparation 


## Part A1 -----
## Count of FT Instructional staff by tenure status, academic rank, and race/ethnicity/gender

#include all possible combinations even if count = 0

#want:
# UNITID=nnnnnn,SURVSECT=HR1,PART=A1,TENURE=1,RANK=1,RACEETHNICITYGENDER=1,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=A1,TENURE=1,RANK=1,RACEETHNICITYGENDER=2,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=A1,TENURE=1,RANK=2,RACEETHNICITYGENDER=1,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=A1,TENURE=1,RANK=2,RACEETHNICITYGENDER=2,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=A1,TENURE=2,RANK=1,RACEETHNICITYGENDER=1,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=A1,TENURE=2,RANK=1,RACEETHNICITYGENDER=2,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=A1,TENURE=6,RANK=7,RACEETHNICITYGENDER=2,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=A1,TENURE=7,RANK=1,RACEETHNICITYGENDER=2,COUNT=nnnnn



##### TO DO ##### Figure out how to sum things for the last expand.grid addition (see IPEDS notes)

#set up the grid of options
combos_A1 <- expand.grid(Unitid = ipeds_unitid, 
                         Tenure = c(1:7), 
                         Rank = c(1:6),
                         REG = c(1:18),
                         Count = 0) %>%
      rbind(expand.grid(Unitid = ipeds_unitid,
                         Tenure = 6,
                         Rank = 7, 
                         REG = 1:18,
                         Count = 0))

#produce the uploadable format
partA1 <- ipeds_df %>%
  
  filter(Instructional == 1,
         FtPt == 'F') #%>%
  select(Unitid,
         Tenure,
         Rank,
         REG,
         Count) #%>%
  
  #add extra combinations
  rbind(combos_A1) #%>%
  
  #aggregate the full data
  group_by(Unitid, Tenure, Rank, REG) %>%
  summarize(Count = sum(Count)) %>% ungroup() %>%
  
  #sort for easy viewing
  arrange(Tenure,
          Rank,
          REG) %>%
  
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=HR",
         PART = "PART=A1",
         TENURE = paste0("TENURE=", Tenure),
         RANK = paste0("RANK=", Rank),
         RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", REG),
         COUNT = paste0("COUNT=", Count)) %>% 
  select(UNITID, 
         SURVSECT, 
         PART, 
         TENURE, 
         RANK, 
         RACEETHNICITYGENDER, 
         COUNT)


#just this part
write.table(x = partA1, sep=",", 
            file= paste0(path, "HumanResources_PartA1.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

#the upload doc
write.table(x = partA1, sep=",", 
            file=paste0(path, "HumanResources_PartsAll.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)


#########################


## Part A2 -----
## Count of FT instructional staff by tenure status, medical school, and function

#include all possible combinations even if count = 0

#want: 
# UNITID=nnnnnn,SURVSECT=HR1,PART=A2,TENURE=1,ISMEDICAL=0,INSTFUNCTION=1,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=A2,TENURE=1,ISMEDICAL=0,INSTFUNCTION=2,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=A2,TENURE=1,ISMEDICAL=1,INSTFUNCTION=1,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=A2,TENURE=1,ISMEDICAL=1,INSTFUNCTION=2,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=A2,TENURE=2,ISMEDICAL=0,INSTFUNCTION=1,COUNT=nnnnn



#set up the grid of options
combos_A2 <- expand.grid(Unitid = ipeds_unitid, 
                         Tenure = c(1:7), 
                         IsMedical = c(0:1),
                         InstFunction = c(1:3, 5),  #4 is generated by the system (subtotal of 1:3)
                         Count = 0)

#produce the uploadable format
partA2 <- ipeds_df %>%
  
  filter(Instructional == 1, #instructional
         FtPt == 'F') %>%
  select(Unitid,
         Tenure,
         IsMedical,
         InstFunction,
         Count) %>%
  
  #add extra combinations
  rbind(combos_A2) %>%
  
  #aggregate the full data
  group_by(Unitid, Tenure, IsMedical, InstFunction) %>%
  summarize(Count = sum(Count)) %>% 
  ungroup() %>%
  
  #sort for easy viewing
  arrange(Tenure,
          IsMedical,
          InstFunction) %>%
  
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=HR",
         PART = "PART=A2",
         TENURE = paste0("TENURE=", Tenure),
         ISMEDICAL = paste0("ISMEDICAL=", IsMedical),
         INSTFUNCTION = paste0("INSTFUNCTION=", InstFunction),
         COUNT = paste0("COUNT=", Count)) %>% 
  select(UNITID, 
         SURVSECT, 
         PART, 
         TENURE, 
         ISMEDICAL, 
         INSTFUNCTION, 
         COUNT)


#just this part
write.table(x = partA2, sep=",", 
            file= paste0(path, "HumanResources_PartA2.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

#the upload doc
write.table(x = partA2, sep=",", 
            file=paste0(path, "HumanResources_PartsAll.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)



###########################


## Part B1 -----
## Count of FT Non-instructional staff by occupational category

#include all possible combinations even if count = 0


# UNITID=nnnnnn,SURVSECT=HR1,PART=B1,OCCCATEGORY1=1,RACEETHNICITYGENDER=1,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=B1,OCCCATEGORY1=1,RACEETHNICITYGENDER=2,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=B1,OCCCATEGORY1=2,RACEETHNICITYGENDER=1,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=B1,OCCCATEGORY1=2,RACEETHNICITYGENDER=2,COUNT=nnnnn


#set up the grid of options
combos_B1 <- expand.grid(Unitid = ipeds_unitid, 
                         OccCategory1 = c(1:17), 
                         REG = c(1:18),
                         Count = 0)

#produce the uploadable format
partB1 <- ipeds_df %>%
  filter(Instructional == 0, #non-instructional
         FtPt == 'F') %>%
  select(Unitid,
         OccCategory1,
         REG,
         Count) %>%
  
  #add extra combinations
  rbind(combos_B1) %>%
  
  #aggregate the full data
  group_by(Unitid, OccCategory1, REG) %>%
  summarize(Count = sum(Count)) %>% 
  ungroup() %>%
  
  #sort for easy viewing
  arrange(OccCategory1,
          REG) %>%
  
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=HR",
         PART = "PART=B1",
         OCCCATEGORY1 = paste0("OCCCATEGORY1=", OccCategory1),
         RACEETHNICITYGENDER = paste0("RACEETHICITYGENDER=", REG),
         COUNT = paste0("COUNT=", Count)) %>% 
  select(UNITID, 
         SURVSECT, 
         PART, 
         OCCCATEGORY1,
         RACEETHNICITYGENDER,
         COUNT)


#just this part
write.table(x = partB1, sep=",", 
            file= paste0(path, "HumanResources_PartB1.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

#the upload doc
write.table(x = partB1, sep=",", 
            file=paste0(path, "HumanResources_PartsAll.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

## Part B2 ----- 
## Full-time non-instructional staff by tenure, medical school, and occupational category

#include all possible combinations even if count = 0

# UNITID=nnnnnn,SURVSECT=HR1,PART=B2,TENURE=1,ISMEDICAL=0,OCCCATEGORY1=2,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=B2,TENURE=1,ISMEDICAL=0,OCCCATEGORY1=3,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=B2,TENURE=1,ISMEDICAL=1,OCCCATEGORY1=2,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=B2,TENURE=1,ISMEDICAL=1,OCCCATEGORY1=3,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=B2,TENURE=2,ISMEDICAL=0,OCCCATEGORY1=2,COUNT=nnnnn

#set up the grid of options
combos_B2 <- expand.grid(Unitid = ipeds_unitid, 
                         Tenure = c(1:7),
                         IsMedical = c(0:1),
                         OccCategory1 = c(2:12),
                         Count = 0)

#produce the uploadable format
partB2 <- ipeds_df %>%
  
  filter(Instructional == 0,
         FtPt == 'F',
         OccCategory1 %in% c(2:12)) %>%
  select(Unitid,
         Tenure,
         IsMedical,
         OccCategory1,
         Count) %>%
  
  #add extra combinations
  rbind(combos_B2) %>%
  
  #aggregate the full data
  group_by(Unitid, Tenure, IsMedical, OccCategory1) %>%
  summarize(Count = sum(Count)) %>% 
  ungroup() %>%
  
  #sort for easy viewing
  arrange(Tenure,
          IsMedical,
          OccCategory1) %>%
  
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=HR",
         PART = "PART=B2",
         TENURE = paste0("TENURE=", Tenure),
         ISMEDICAL = paste0("ISMEDICAL=", IsMedical),
         OCCCATEGORY1 = paste0("OCCCATEGORY1=", OccCategory1),
         COUNT = paste0("COUNT=", Count)) %>% 
  select(UNITID, 
         SURVSECT, 
         PART, 
         TENURE,
         ISMEDICAL,
         OCCCATEGORY1,
         COUNT)


#just this part
write.table(x = partB2, sep=",", 
            file= paste0(path, "HumanResources_PartB2.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

#the upload doc
write.table(x = partB2, sep=",", 
            file=paste0(path, "HumanResources_PartsAll.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)


## Part B3 ----- 
## Full-time non-instructional staff by medical school, and occupational category

#include all possible combinations even if count = 0

# UNITID=nnnnnn,SURVSECT=HR1,PART=B3,ISMEDICAL=0,OCCCATEGORY1=2,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=B3,ISMEDICAL=0,OCCCATEGORY1=3,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=B3,ISMEDICAL=1,OCCCATEGORY1=2,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=B3,ISMEDICAL=1,OCCCATEGORY1=3,COUNT=nnnnn

#set up the grid of options
combos_B3 <- expand.grid(Unitid = ipeds_unitid,
                         IsMedical = c(0:1),
                         OccCategory1 = c(13:17),
                         Count = 0)

#produce the uploadable format
partB3 <- ipeds_df %>%
  
  filter(Instructional == 0,
         FtPt == 'F',
         OccCategory1 %in% c(13:17)) %>%
  select(Unitid,
         IsMedical,
         OccCategory1,
         Count) %>%
  
  #add extra combinations
  rbind(combos_B3) %>%
  
  #aggregate the full data
  group_by(Unitid, IsMedical, OccCategory1) %>%
  summarize(Count = sum(Count)) %>% 
  ungroup() %>%
  
  #sort for easy viewing
  arrange(IsMedical,
          OccCategory1) %>%
  
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=HR",
         PART = "PART=B3",
         ISMEDICAL = paste0("ISMEDICAL=", IsMedical),
         OCCCATEGORY1 = paste0("OCCCATEGORY1=", OccCategory1),
         COUNT = paste0("COUNT=", Count)) %>% 
  select(UNITID, 
         SURVSECT, 
         PART, 
         ISMEDICAL,
         OCCCATEGORY1,
         COUNT)


#just this part
write.table(x = partB3, sep=",", 
            file= paste0(path, "HumanResources_PartB3.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

#the upload doc
write.table(x = partB3, sep=",", 
            file=paste0(path, "HumanResources_PartsAll.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)


## Part D1 ----- 
## Part-time staff by occupational category
# Sort order: UNITID,SURVSECT,PART,OCCCATEGORY1,RACEETHNICITYGENDER.

#include all possible combinations even if count = 0

# UNITID=nnnnnn,SURVSECT=HR1,PART=D1,OCCCATEGORY1=1,RACEETHNICITYGENDER=1,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=D1,OCCCATEGORY1=1,RACEETHNICITYGENDER=2,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=D1,OCCCATEGORY1=2,RACEETHNICITYGENDER=1,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=D1,OCCCATEGORY1=2,RACEETHNICITYGENDER=2,COUNT=nnnnn


#set up the grid of options
combos_D1 <- expand.grid(Unitid = ipeds_unitid,
                         OccCategory1 = c(1:18),
                         REG = c(1:18),
                         Count = 0)

#produce the uploadable format
partD1 <- ipeds_df %>%
  
  filter(FtPt == 'P',
         OccCategory1 %in% c(1:18)) %>%
  select(Unitid,
         OccCategory1,
         REG,
         Count) %>%
  
  #add extra combinations
  rbind(combos_D1) %>%
  
  #aggregate the full data
  group_by(Unitid, OccCategory1, REG) %>%
  summarize(Count = sum(Count)) %>% 
  ungroup() %>%
  
  #sort for easy viewing
  arrange(OccCategory1, 
          REG) %>%
  
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=HR",
         PART = "PART=D1",
         OCCCATEGORY1 = paste0("OCCCATEGORY1=", OccCategory1),
         RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", REG),
         COUNT = paste0("COUNT=", Count)) %>% 
  select(UNITID, 
         SURVSECT, 
         PART, 
         OCCCATEGORY1,
         RACEETHNICITYGENDER,
         COUNT)


#just this part
write.table(x = partD1, sep=",", 
            file= paste0(path, "HumanResources_PartD1.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

#the upload doc
write.table(x = partD1, sep=",", 
            file=paste0(path, "HumanResources_PartsAll.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)


## Part D2 ----- 
## Graduate assistants by occupational category and race/ethnicity/gender

#include all possible combinations even if count = 0


# UNITID=nnnnnn,SURVSECT=HR1,PART=D2,OCCCATEGORY4=1,RACEETHNICITYGENDER=1,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=D2,OCCCATEGORY4=1,RACEETHNICITYGENDER=2,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=D2,OCCCATEGORY4=2,RACEETHNICITYGENDER=1,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=D2,OCCCATEGORY4=2,RACEETHNICITYGENDER=2,COUNT=nnnnn

#set up the grid of options
combos_D2 <- expand.grid(Unitid = ipeds_unitid,
                         OccCategory4 = c(1:3),
                         REG = c(1:18),
                         Count = 0)

#produce the uploadable format
partD2 <- ipeds_df %>%
  
  filter(OccCategory4 %in% c(1:3)) %>%
  select(Unitid,
         OccCategory4,
         REG,
         Count) %>%
  
  #add extra combinations
  rbind(combos_D2) %>%
  
  #aggregate the full data
  group_by(Unitid, OccCategory4, REG) %>%
  summarize(Count = sum(Count)) %>% 
  ungroup() %>%
  
  #sort for easy viewing
  arrange(OccCategory4, 
          REG) %>%
  
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=HR",
         PART = "PART=D2",
         OCCCATEGORY4 = paste0("OCCCATEGORY4=", OccCategory4),
         RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", REG),
         COUNT = paste0("COUNT=", Count)) %>% 
  select(UNITID, 
         SURVSECT, 
         PART, 
         OCCCATEGORY4,
         RACEETHNICITYGENDER,
         COUNT)


#just this part
write.table(x = partD2, sep=",", 
            file= paste0(path, "HumanResources_PartD2.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

#the upload doc
write.table(x = partD2, sep=",", 
            file=paste0(path, "HumanResources_PartsAll.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)


## Part D3 ----- 
## Part-time staff by tenure, medical school, and occupational category

#include all possible combinations even if count = 0

# UNITID=nnnnnn,SURVSECT=HR1,PART=D3,TENURE=1,ISMEDICAL=0,OCCCATEGORY3=2,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=D3,TENURE=1,ISMEDICAL=0,OCCCATEGORY3=3,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=D3,TENURE=1,ISMEDICAL=1,OCCCATEGORY3=2,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=D3,TENURE=1,ISMEDICAL=1,OCCCATEGORY3=3,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=D3,TENURE=2,ISMEDICAL=0,OCCCATEGORY3=2,COUNT=nnnnn


#set up the grid of options
combos_D3 <- expand.grid(Unitid = ipeds_unitid,
                         Tenure = c(1:7),
                         IsMedical = c(0:1),
                         OccCategory3 = c(1:15),
                         Count = 0)

#produce the uploadable format
partD3 <- ipeds_df %>%
  
  filter(FtPt == "P",
         OccCategory3 %in% c(1:15)) %>%
  select(Unitid,
         Tenure,
         IsMedical,
         OccCategory3,
         Count) %>%
  
  #add extra combinations
  rbind(combos_D3) %>%
  
  #aggregate the full data
  group_by(Unitid, Tenure, IsMedical, OccCategory3) %>%
  summarize(Count = sum(Count)) %>% 
  ungroup() %>%
  
  #sort for easy viewing
  arrange(Tenure,
          IsMedical,
          OccCategory3) %>%
  
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=HR",
         PART = "PART=D3",
         TENURE = paste0("TENURE=", Tenure),
         ISMEDICAL = paste0("ISMEDICAL=", IsMedical),
         OCCCATEGORY3 = paste0("OCCCATEGORY3=", OccCategory3),
         COUNT = paste0("COUNT=", Count)) %>% 
  select(UNITID, 
         SURVSECT, 
         PART, 
         TENURE,
         ISMEDICAL,
         OCCCATEGORY3,
         COUNT)


#just this part
write.table(x = partD3, sep=",", 
            file= paste0(path, "HumanResources_PartD3.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

#the upload doc
write.table(x = partD3, sep=",", 
            file=paste0(path, "HumanResources_PartsAll.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

## Part D4 ----- 
## Part-time Non-instructional staff by medical school, and occupational category

#include all possible combinations even if count = 0


# UNITID=nnnnnn,SURVSECT=HR1,PART=D4,ISMEDICAL=0,OCCCATEGORY3=16,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=D4,ISMEDICAL=0,OCCCATEGORY3=17,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=D4,ISMEDICAL=1,OCCCATEGORY3=16,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=D4,ISMEDICAL=1,OCCCATEGORY3=17,COUNT=nnnnn

#set up the grid of options
combos_D4 <- expand.grid(Unitid = ipeds_unitid,
                         IsMedical = c(0:1),
                         OccCategory3 = c(16:20, 22:24),
                         Count = 0)

#produce the uploadable format
partD4 <- ipeds_df %>%
  
  filter(FtPt == "P",
         OccCategory3 %in% c(16:20, 22:24)) %>%
  select(Unitid,
         IsMedical,
         OccCategory3,
         Count) %>%
  
  #add extra combinations
  rbind(combos_D4) %>%
  
  #aggregate the full data
  group_by(Unitid, IsMedical, OccCategory3) %>%
  summarize(Count = sum(Count)) %>% 
  ungroup() %>%
  
  #sort for easy viewing
  arrange(IsMedical,
          OccCategory3) %>%
  
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=HR",
         PART = "PART=D4",
         ISMEDICAL = paste0("ISMEDICAL=", IsMedical),
         OCCCATEGORY3 = paste0("OCCCATEGORY3=", OccCategory3),
         COUNT = paste0("COUNT=", Count)) %>% 
  select(UNITID, 
         SURVSECT, 
         PART, 
         ISMEDICAL,
         OCCCATEGORY3,
         COUNT)


#just this part
write.table(x = partD4, sep=",", 
            file= paste0(path, "HumanResources_PartD4.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

#the upload doc
write.table(x = partD4, sep=",", 
            file=paste0(path, "HumanResources_PartsAll.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)


## Part G1 ----- 
## Salaries of Instructional staff

#include all possible combinations even if count = 0


# UNITID=nnnnnn,SURVSECT=HR1,PART=G1,RANK=1,GENDER=1,12MCOUNT=nnnnn,11MCOUNT=nnnnn,
# 10MCOUNT=nnnnn,9MCOUNT=nnnnn, LESSTHAN9MCOUNT=nnnnn, 12MSOUTLAYS=nnnnnnnnnnnn,
# 11MSOUTLAYS=nnnnnnnnnnnn, 10MSOUTLAYS=nnnnnnnnnnnn,9MSOUTLAYS=nnnnnnnnnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=G1,RANK=1,GENDER=2,12MCOUNT=nnnnn,11MCOUNT=nnnnn,
# 10MCOUNT=nnnnn,9MCOUNT=nnnnn, LESSTHAN9MCOUNT=nnnnn, 12MSOUTLAYS=nnnnnnnnnnnn,
# 11MSOUTLAYS=nnnnnnnnnnnn, 10MSOUTLAYS=nnnnnnnnnnnn,9MSOUTLAYS=nnnnnnnnnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=G1,RANK=2,GENDER=1,12MCOUNT=nnnnn,11MCOUNT=nnnnn,
# 10MCOUNT=nnnnn,9MCOUNT=nnnnn, LESSTHAN9MCOUNT=nnnnn, 12MSOUTLAYS=nnnnnnnnnnnn,
# 11MSOUTLAYS=nnnnnnnnnnnn, 10MSOUTLAYS=nnnnnnnnnnnn,9MSOUTLAYS=nnnnnnnnnnnn



#set up the grid of options
combos_G1 <- expand.grid(Unitid = ipeds_unitid,
                         Rank = c(1:6),
                         Gender = c(1:2),
                         Months = c(8:12),
                         Salary = 0,
                         EmpId = dummy_id,
                         Count = 0)

#produce the uploadable format
partG1 <- ipeds_df %>%
  
  filter(Instructional == 1, #instructional 
         FtPt == "F") %>%  #not listed in uploadable instructions -- verify?
  select(Unitid,
         Rank,
         Gender,
         Months,
         Salary,
         EmpId,
         Count) %>%
  
  #add extra combinations
  rbind(combos_G1) %>%
  
  #reshape months and salary
  mutate(months_count = recode(Months,
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
                             .default = 'ZzSoutlays')) %>%
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
  arrange(Rank,
          Gender) %>%
  
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=HR",
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
         `9mSoutlays` = paste0("9MSOUTLAYS=", `9mSoutlays`),
         `ZzSoutlays` = paste0("ZzSoutlays=", ZzSoutlays)) %>% #here for dubugging purposes
  select(UNITID, 
         SURVSECT, 
         PART, 
         RANK,
         GENDER,
         `12mCount`,
         `11mCount`,
         `10mCount`,
         `9mCount`,
         LessThan9mCount,
         #ZzCount,
         `12mSoutlays`,
         `11mSoutlays`,
         `10mSoutlays`,
         `9mSoutlays`#,
         #`ZzSoutlays` #not needed for ipeds reporting
         )


#just this part
write.table(x = partG1, sep=",", 
            file= paste0(path, "HumanResources_PartG1.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

#the upload doc
write.table(x = partG1, sep=",", 
            file=paste0(path, "HumanResources_PartsAll.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)



## Part G2 ----- 
## Salaries of non-instructional staff

#include all possible combinations even if count = 0


# UNITID=nnnnnn,SURVSECT=HR1,PART=G2,OCCCATEGORY2=1,SOUTLAYS=nnnnnnnnnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=G2,OCCCATEGORY2=2,SOUTLAYS=nnnnnnnnnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=G2,OCCCATEGORY2=3,SOUTLAYS=nnnnnnnnnnnn



#set up the grid of options
combos_G2 <- expand.grid(Unitid = ipeds_unitid,
                         OccCategory2 = c(1:13),
                         Salary = 0,
                         EmpId = dummy_id,
                         Count = 0)

#produce the uploadable format
partG2 <- ipeds_df %>%
  
  filter(Instructional == 0, #non-instructional
         FtPt == "F") %>%  #not listed in uploadable instructions -- verify?
  select(Unitid,
         OccCategory2,
         Salary,
         EmpId,
         Count) %>%
  
  #add extra combinations
  rbind(combos_G2) %>%
  
  #aggregate the full data
  group_by(Unitid, OccCategory2) %>%
  summarize(Salary = sum(Salary)) %>%
  ungroup() %>%
  
  #sort for easy viewing
  arrange(OccCategory2) %>%
  
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=HR",
         PART = "PART=G2",
         OCCCATEGORY2 = paste0("OCCCATEGORY2=", OccCategory2),
         SOUTLAYS = paste0("SOUTLAYS=", Salary)) %>% 
  select(UNITID, 
         SURVSECT, 
         PART,
         OCCCATEGORY2,
         SOUTLAYS)


#just this part
write.table(x = partG2, sep=",", 
            file= paste0(path, "HumanResources_PartG2.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

#the upload doc
write.table(x = partG2, sep=",", 
            file=paste0(path, "HumanResources_PartsAll.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)


## Part H1 ----- 
## Full-time new hire instructional staff by tenure status and race/ethnicity/gender

#include all possible combinations even if count = 0

# UNITID=nnnnnn,SURVSECT=HR1,PART=H1,TENURE=1,RACEETHNICITYGENDER=1,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=H1,TENURE=1,RACEETHNICITYGENDER=2,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=H1,TENURE=2,RACEETHNICITYGENDER=1,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=H1,TENURE=2,RACEETHNICITYGENDER=2,COUNT=nnnnn


#set up the grid of options
combos_H1 <- expand.grid(Unitid = ipeds_unitid, 
                         Tenure = c(1:7),
                         REG = c(1:18),
                         Count = 0)

#produce the uploadable format
partH1 <- ipeds_df %>%
  
  filter(Instructional == 1, 
         NewHire == 1,
         FtPt == 'F') %>%
  select(Unitid,
         Tenure,
         REG,
         Count) %>%
  
  #add extra combinations
  rbind(combos_H1) %>%
  
  #aggregate the full data
  group_by(Unitid, Tenure, REG) %>%
  summarize(Count = sum(Count)) %>% ungroup() %>%
  
  #sort for easy viewing
  arrange(Tenure,
          REG) %>%
  
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=HR",
         PART = "PART=H1",
         TENURE = paste0("TENURE=", Tenure),
         RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", REG),
         COUNT = paste0("COUNT=", Count)) %>% 
  select(UNITID, 
         SURVSECT, 
         PART, 
         TENURE, 
         RACEETHNICITYGENDER, 
         COUNT)


#just this part
write.table(x = partH1, sep=",", 
            file= paste0(path, "HumanResources_PartH1.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

#the upload doc
write.table(x = partH1, sep=",", 
            file=paste0(path, "HumanResources_PartsAll.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)




## Part H2 ----- 
## New hires by occupational category, Race/Ethnicity/Gender

#include all possible combinations even if count = 0


# UNITID=nnnnnn,SURVSECT=HR1,PART=H2,OCCCATEGORY5=1,RACEETHNICITYGENDER=1,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=H2,OCCCATEGORY5=1,RACEETHNICITYGENDER=2,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=H2,OCCCATEGORY5=2,RACEETHNICITYGENDER=1,COUNT=nnnnn
# UNITID=nnnnnn,SURVSECT=HR1,PART=H2,OCCCATEGORY5=2,RACEETHNICITYGENDER=2,COUNT=nnnnn


#set up the grid of options
combos_H2 <- expand.grid(Unitid = ipeds_unitid, 
                         OccCategory5 = c(2:14),
                         REG = c(1:18),
                         Count = 0)

#produce the uploadable format
partH2 <- ipeds_df %>%
  
  filter(NewHire == 1,
         FtPt == 'Full Time',
         OccCategory5 %in% c(2:14)) %>%
  select(Unitid,
         OccCategory5,
         REG,
         Count) %>%
  
  #add extra combinations
  rbind(combos_H2) %>%
  
  #aggregate the full data
  group_by(Unitid, OccCategory5, REG) %>%
  summarize(Count = sum(Count)) %>% ungroup() %>%
  
  #sort for easy viewing
  arrange(OccCategory5,
          REG) %>%
  
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=HR",
         PART = "PART=H2",
         OCCCATEGORY5 = paste0("OCCCATEGORY5=", OccCategory5),
         RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", REG),
         COUNT = paste0("COUNT=", Count)) %>% 
  select(UNITID, 
         SURVSECT, 
         PART,
         OCCCATEGORY5,
         RACEETHNICITYGENDER, 
         COUNT)


#just this part
write.table(x = partH2, sep=",", 
            file= paste0(path, "HumanResources_PartH2.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

#the upload doc
write.table(x = partH2, sep=",", 
            file=paste0(path, "HumanResources_PartsAll.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)



#################
## Status message: finished

svDialogs::dlg_message(paste0("Human Resources file available. Please see results at ", path))




