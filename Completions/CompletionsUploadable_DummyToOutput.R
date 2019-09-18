#### 
## Completions Uploadable
## Producing a key-value text file
####


##TO DO
## Could move the CIP > 6 digits code up top 
##   and apply to the startingdf and the extracips objects before any other processing; 
##   then it doesn't have to go into the individual part calculations 


library(tidyverse)

#set an output path:
# path <- "C:/Users/alanski/Documents/Bitbucket/comparative/IPEDS Uploadables/"

#set a date to calculate age
#set the date in the format that will match your student data
library(lubridate)
completions_date <- dmy("21-9-2019")


#if testing: run dummy data file
source("/Volumes/Staff/Groups/Decision Support/Exchange/Shiloh/IPEDS-Uploadables/Completions/CompletionsStartingDf_DummyData.R")

## Part A

#want: 
# UNITID=nnnnnn,SURVSECT=COM,PART=A,MAJORNUM=1,CIPCODE=01.0101,AWLEVEL=1,RACE=1,SEX=1,COUNT=nnnnn 
# UNITID=nnnnnn,SURVSECT=COM,PART=A,MAJORNUM=1,CIPCODE=01.0101,AWLEVEL=1,RACE=1,SEX=2,COUNT=nnnnn 
# UNITID=nnnnnn,SURVSECT=COM,PART=A,MAJORNUM=1,CIPCODE=01.0101,AWLEVEL=2,RACE=2,SEX=1,COUNT=nnnnn 
#null record: 
# UNITID=nnnnnn,SURVSECT=COM,PART=A,MAJORNUM=1,CIPCODE=xx.xxxx,AWLEVEL=i,RACE=1,SEX=1,COUNT=0.

# head(startingdf)
# head(extracips)

source("/Volumes/Staff/Groups/Decision Support/Exchange/Shiloh/R/utilities_standard_header.R")

startingdf <- get_query_from_file(path = "/Volumes/Staff/Groups/Decision Support/Exchange/Shiloh/SQL/",
                                  file_name = "ipeds_completions_2019-08-30")

startingdf$BIRTHDATE <- as.Date(substr(startingdf$BIRTHDATE, 1, 10))
startingdf$MAJORNUMBER <- as.numeric(startingdf$MAJORNUMBER)
startingdf <- startingdf %>%
  mutate(DEGREELEVEL = replace(DEGREELEVEL, DEGREELEVEL == "13", "5"),
         DEGREELEVEL = replace(DEGREELEVEL, DEGREELEVEL == "14", "6"),
         DEGREELEVEL = replace(DEGREELEVEL, DEGREELEVEL == "17", "7"),
         DEGREELEVEL = replace(DEGREELEVEL, DEGREELEVEL == "18", "8"),
         DEGREELEVEL = replace(DEGREELEVEL, DEGREELEVEL == "21", "17")) %>%
  mutate(RACEETHNICITY = replace(RACEETHNICITY, RACEETHNICITY == "NONRS", "1"),
         RACEETHNICITY = replace(RACEETHNICITY, RACEETHNICITY == "HISPA", "2"),
         RACEETHNICITY = replace(RACEETHNICITY, RACEETHNICITY == "AIAKN", "3"),
         RACEETHNICITY = replace(RACEETHNICITY, RACEETHNICITY == "ASIAN", "4"),
         RACEETHNICITY = replace(RACEETHNICITY, RACEETHNICITY == "BLACK", "5"),
         RACEETHNICITY = replace(RACEETHNICITY, RACEETHNICITY == "PACIF", "6"),
         RACEETHNICITY = replace(RACEETHNICITY, RACEETHNICITY == "WHITE", "7"),
         RACEETHNICITY = replace(RACEETHNICITY, RACEETHNICITY == "MULTI", "8"),
         RACEETHNICITY = replace(RACEETHNICITY, RACEETHNICITY == "UNKWN", "9")) %>%
  mutate(SEX = replace(SEX, SEX == "M", "1"),
         SEX = replace(SEX, SEX == "F", "2"))
names(startingdf) <- c("Unitid", 
                       "StudentId",
                       "RaceEthnicity",
                       "Sex",
                       "DegreeLevel",
                       "MajorNumber",
                       "MajorCip",
                       "DistanceEd",
                       "Birthdate")

#prep the extra cips
extracips_A <- extracips %>% 
  select(Unitid,
         MajorNumber,
         MajorCip,
         DegreeLevel,
         RaceEthnicity,
         Sex, 
         Count)


         
partA <- startingdf %>%
  
  #prep the full data
  group_by(Unitid, MajorNumber, MajorCip, DegreeLevel, RaceEthnicity, Sex) %>%
  summarize(Count = n()) %>% ungroup() %>%
  
  #add cips
  rbind(extracips_A) %>%
  
  #get 6-digit cips
  separate(col = MajorCip, into = c("Two", "Four"), sep = "\\.") %>%
  mutate(Two = ifelse(nchar(Two) == 1, 
                      paste0("0", Two),
                      Two),
         Four = ifelse(nchar(Four) == 1,
                       paste0(Four, "000"),
                       ifelse(nchar(Four) == 2, 
                              paste0(Four, "00"),
                              ifelse(nchar(Four) == 3,
                                     paste0(Four, "0"),
                                     Four)))) %>%
  
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=COM",
         PART = "PART=A",
         MAJORNUM = paste0("MAJORNUM=", MajorNumber),
         CIPCODE = paste0("CIPCODE=", Two, ".", Four),
         AWLEVEL = paste0("AWLEVEL=", DegreeLevel),
         RACE = paste0("RACE=", RaceEthnicity),
         SEX = paste0("SEX=", Sex),
         COUNT = paste0("COUNT=", Count)) %>% 
  select(UNITID, SURVSECT, PART, MAJORNUM, CIPCODE, AWLEVEL, RACE, SEX, COUNT) %>%
  arrange(UNITID, SURVSECT, PART, MAJORNUM, CIPCODE, AWLEVEL, RACE, SEX)



write.table(x = partA, sep=",", 
            file= paste0(path, "Completions_PartA_test.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

write.table(x = partA, sep=",", 
            file=paste0(path, "Completions_PartsAll_test.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)


#########################

## Part B
#want:
# UNITID=nnnnnn,SURVSECT=COM,PART=B,MAJORNUM=1,CIPCODE=01.0101,AWLEVEL=3,DistanceED=1 
# UNITID=nnnnnn,SURVSECT=COM,PART=B,MAJORNUM=1,CIPCODE=01.0101,AWLEVEL=5,DistanceED=2 
# UNITID=nnnnnn,SURVSECT=COM,PART=B,MAJORNUM=1,CIPCODE=01.0101,AWLEVEL=i,DistanceED=n 


extracips_B <- extracips %>%
  select(Unitid, 
         MajorNumber,
         MajorCip,
         DegreeLevel,
         DistanceEd)

partB <- startingdf %>%
  select(Unitid, MajorNumber, MajorCip, DegreeLevel, DistanceEd) %>%
  unique() %>%
  
  #if we need to add the extra cips, do it here
  rbind(extracips_B) %>%
  
  #get 6-digit cips
  separate(col = MajorCip, into = c("Two", "Four"), sep = "\\.") %>%
  mutate(Two = ifelse(nchar(Two) == 1, 
                      paste0("0", Two),
                      Two),
         Four = ifelse(nchar(Four) == 1,
                       paste0(Four, "000"),
                       ifelse(nchar(Four) == 2, 
                              paste0(Four, "00"),
                              ifelse(nchar(Four) == 3,
                                     paste0(Four, "0"),
                                     Four)))) %>%
  
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=COM",
         PART = "PART=B",
         MAJORNUM = paste0("MAJORNUM=", MajorNumber),
         CIPCODE = paste0("CIPCODE=", Two, ".", Four),
         AWLEVEL = paste0("AWLEVEL=", DegreeLevel),
         DistanceED = paste0("DistanceED=", DistanceEd)) %>% 
  select(UNITID, SURVSECT, PART, MAJORNUM, CIPCODE, AWLEVEL, DistanceED) %>%
  arrange(UNITID, SURVSECT, PART, MAJORNUM, CIPCODE, AWLEVEL, DistanceED)


write.table(x = partB, sep=",", 
            file= paste0(path, "Completions_PartB_test.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

write.table(x = partB, sep=",", 
            file=paste0(path, "Completions_PartsAll_test.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)


#########################

## Part C
#want:
#UNITID=nnnnnn,SURVSECT=COM,PART=C,RACE=2,SEX=1,COUNT=nnnnn 
#UNITID=nnnnnn,SURVSECT=COM,PART=C,RACE=2,SEX=2,COUNT=nnnnn 
#UNITID=nnnnnn,SURVSECT=COM,PART=C,RACE=j,SEX=k,COUNT=nnnnn 

#is this counting STUDENTS not degrees?  if so, deduplicate first

partC <- startingdf %>%
  select(Unitid, StudentId, RaceEthnicity, Sex) %>%
  unique() %>%
  group_by(Unitid, RaceEthnicity, Sex) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=COM",
         PART = "PART=C",
         RACE = paste0("RACE=", RaceEthnicity),
         SEX = paste0("SEX=", Sex),
         COUNT = paste0("COUNT=", Count)) %>% 
  select(UNITID, SURVSECT, PART, RACE, SEX, COUNT) %>%
  arrange(UNITID, SURVSECT, PART, RACE, SEX)


#if distinct students, should be 100 (yes)
write.table(x = partC, sep=",", 
            file= paste0(path, "Completions_PartC_test.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

write.table(x = partC, sep=",", 
            file=paste0(path, "Completions_PartsAll_test.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)




#########################

## Part D
#want:
#UNITID=nnnnnn,SURVSECT=COM,PART=D,CTLEVEL=3,CRACE15=nnnnn,CRACE16=nnnnn,CRACE41=nnnnn...


#prep the extra levels
extralevel_D <- extracips %>% 
  select(Unitid,
         DegreeLevel) %>% 
  unique() %>%
  filter(!(DegreeLevel %in% startingdf$DegreeLevel)) %>%
  mutate(StudentId = 9999,
         RaceEthnicity = 1, 
         Sex = 1, 
         Birthdate = ymd("1900-01-01"),
         CountRE = 0,
         CountSex = 0,
         CountAge = 0) %>%
  select(Unitid, StudentId, everything())



#need to set a calculation date for age

partD <- startingdf %>%
  select(Unitid, StudentId, DegreeLevel, RaceEthnicity, Sex, Birthdate) %>%
  
  #add values to sum later
  mutate(CountRE = 1, CountSex = 1, CountAge = 1) %>%
  
  #add any extra levels
  rbind(extralevel_D) %>%
  
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
  
  #recode and spread to get IPEDS columns
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
                                .default = "ZRACEETH")) %>%
  mutate(Age = (completions_date-Birthdate)/dyears(1)) %>% 
  mutate(AgeGroup = ifelse(Age < 18, "AGE1",
                        ifelse(Age <= 24, "AGE2",
                              ifelse(Age <= 39, "AGE3",
                                      ifelse(Age >= 40, "AGE4",
                                             "AGE9"))))) %>%
  mutate(AgeGroup = ifelse(is.na(Age), 
                            "AGE5", 
                            AgeGroup)) %>%
  spread(key = RaceEthnicity, value = CountRE) %>%
  mutate(Sex = recode(Sex,
                      `1` = "CRACE15",
                      `2` = "CRACE16",
                      .default = "ZRACESEX")) %>%
  spread(key = Sex, value = CountSex) %>%
  spread(key = AgeGroup, value = CountAge) %>%
  #add spread columns; extra levels have values of 0
  mutate(AGE1 = ifelse(any(names(.) == "AGE1"), AGE1, 0),
         AGE2 = ifelse(any(names(.) == "AGE2"), AGE2, 0),
         AGE3 = ifelse(any(names(.) == "AGE3"), AGE3, 0),
         AGE4 = ifelse(any(names(.) == "AGE4"), AGE4, 0),
         AGE5 = ifelse(any(names(.) == "AGE5"), AGE5, 0)) %>%
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
            AGE5 = sum(AGE5, na.rm = T)) %>%
  ungroup() %>%
  
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
         CRACE15, CRACE16, 
         CRACE17, CRACE41, CRACE42, CRACE43, 
         CRACE44, CRACE45, CRACE46, CRACE47, CRACE23, 
         AGE1, AGE2, AGE3, AGE4, AGE5) %>%
  arrange(UNITID, SURVSECT, PART, CTLEVEL)


  
#check for default values in spread items 
colnames(partD)

#check for default in level
sort(unique(partD$CTLEVEL))


write.table(x = partD, sep=",", 
            file= paste0(path, "Completions_PartD_test.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

write.table(x = partD, sep=",", 
            file=paste0(path, "Completions_PartsAll_test.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)


