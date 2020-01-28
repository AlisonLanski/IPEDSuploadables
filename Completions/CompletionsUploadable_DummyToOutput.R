#### 
## Completions Uploadable
## Producing a key-value text file
####


#########################################################
###
## Set up variables and prep CIPs to correct format


#load package
library(tidyverse)


#set an output path:
path <- svDialogs::dlg_dir(default = getwd(), title = "Select the file output location")$res 

#make sure the final / is in the path (before the filename)
if(!str_detect(path, pattern = "/$")) {
  path <- paste0(path, "/")
}


#set the school's unitid (for later)
ipeds_unitid  <- svDialogs::dlgInput("What is your school's IPEDS Unitid?")$res


#set a dummy studentID (for later)
# dummy_studentid <- svDialogs::dlgInput("Provide a value that can be used as a dummy-student ID")$res


#if testing: run dummy data file
#source(paste0(path, "CompletionsStartingDf_DummyData.R"))


#prep datafiles: CIP codes to 6-digit correctly
startingdf <- startingdf %>%
  separate(col = MajorCip, 
           into = c("Two", "Four"), 
           sep = "\\.") %>%
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
  mutate(MajorCip = paste0(Two, '.', Four)) %>%
  select(-Two, -Four)


extracips <- extracips %>%
  separate(col = MajorCip, 
           into = c("Two", "Four"), 
           sep = "\\.") %>%
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
  mutate(MajorCip = paste0(Two, '.', Four)) %>%
  select(-Two, -Four)



#####################################################################
####
##    Produce upload files


###
### NOTE 
### On the use of sorting (arrange) in each step below
###
### IPEDS requires a sort on the entire data (Parts A > D) in this order:
### UNITID, SURVSECT, PART, MAJORNUM
###
### By sorting each part separately and appending in order
### this will be taken care of by the script
###
### There is additional sorting in each part 
### to make the final product more person-friendly for reading
###



## Part A --- Count of completers by major number, cip, level, race, and sex

#want: 
# UNITID=nnnnnn,SURVSECT=COM,PART=A,MAJORNUM=1,CIPCODE=01.0101,AWLEVEL=1,RACE=1,SEX=1,COUNT=nnnnn 
# UNITID=nnnnnn,SURVSECT=COM,PART=A,MAJORNUM=1,CIPCODE=01.0101,AWLEVEL=1,RACE=1,SEX=2,COUNT=nnnnn 
# UNITID=nnnnnn,SURVSECT=COM,PART=A,MAJORNUM=1,CIPCODE=01.0101,AWLEVEL=2,RACE=2,SEX=1,COUNT=nnnnn 
#null record: 
# UNITID=nnnnnn,SURVSECT=COM,PART=A,MAJORNUM=1,CIPCODE=xx.xxxx,AWLEVEL=i,RACE=1,SEX=1,COUNT=0.

#Glance at data if needed (exploratory)
#head(startingdf)
#head(extracips)


#prep the extra cips
extracips_A <- extracips %>% 
  select(Unitid,
         MajorNumber,
         MajorCip,
         DegreeLevel,
         RaceEthnicity,
         Sex, 
         Count)
extracips_A <- NULL

#produce the uploadable format
partA <- startingdf %>%
  
  #aggregate the full data
  group_by(Unitid, MajorNumber, MajorCip, DegreeLevel, RaceEthnicity, Sex) %>%
  summarize(Count = n()) %>% ungroup() %>%
  
  #add extra cips
  rbind(extracips_A) %>%
  
  #sort for easy viewing
  arrange(MajorNumber,
          MajorCip,
          DegreeLevel,
          RaceEthnicity,
          Sex) %>%
  
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=COM",
         PART = "PART=A",
         MAJORNUM = paste0("MAJORNUM=", MajorNumber),
         CIPCODE = paste0("CIPCODE=", MajorCip),
         AWLEVEL = paste0("AWLEVEL=", DegreeLevel),
         RACE = paste0("RACE=", RaceEthnicity),
         SEX = paste0("SEX=", Sex),
         COUNT = paste0("COUNT=", Count)) %>% 
  select(UNITID, 
         SURVSECT, 
         PART, 
         MAJORNUM, 
         CIPCODE, 
         AWLEVEL, 
         RACE, 
         SEX, 
         COUNT) #%>%
#sorting is only required for Unitid > CipCode
#the others are included for ease-of-use 
#if the files are viewed by a person
# arrange(UNITID, 
#         SURVSECT, 
#         PART, 
#         MAJORNUM, 
#         CIPCODE, 
#         AWLEVEL, 
#         RACE, 
#         SEX)

#just this part
write.table(x = partA, sep=",", 
            file= paste0(path, "Completions_PartA.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

#the upload doc
write.table(x = partA, sep=",", 
            file=paste0(path, "Completions_PartsAll.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)


#########################

## Part B -- unduplicated list of offerings by major, cip, level, and distanceed status

#want:
# UNITID=nnnnnn,SURVSECT=COM,PART=B,MAJORNUM=1,CIPCODE=01.0101,AWLEVEL=3,DistanceED=1 
# UNITID=nnnnnn,SURVSECT=COM,PART=B,MAJORNUM=1,CIPCODE=01.0101,AWLEVEL=5,DistanceED=2 
# UNITID=nnnnnn,SURVSECT=COM,PART=B,MAJORNUM=1,CIPCODE=01.0101,AWLEVEL=i,DistanceED=n 


#prep extra cip codes
extracips_B <- extracips %>%
  select(Unitid, 
         MajorNumber,
         MajorCip,
         DegreeLevel,
         DistanceEd)


#prep upload
partB <- startingdf %>%
  select(Unitid, MajorNumber, MajorCip, DegreeLevel, DistanceEd) %>%
  unique() %>%
  
  #if we need to add the extra cips, do it here
  rbind(extracips_B) %>%
  
  #sort for easy viewing 
  arrange(MajorNumber,
          MajorCip,
          DegreeLevel,
          DistanceEd) %>%
  
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=COM",
         PART = "PART=B",
         MAJORNUM = paste0("MAJORNUM=", MajorNumber),
         CIPCODE = paste0("CIPCODE=", MajorCip),
         AWLEVEL = paste0("AWLEVEL=", DegreeLevel),
         DistanceED = paste0("DistanceED=", DistanceEd)) %>% 
  select(UNITID, 
         SURVSECT, 
         PART, 
         MAJORNUM, 
         CIPCODE, 
         AWLEVEL, 
         DistanceED) #%>%
#sorting is only required for Unitid > CipCode
#the others are included for ease-of-use 
#if the files are viewed by a person
# arrange(UNITID, 
#         SURVSECT, 
#         PART, 
#         MAJORNUM, 
#         CIPCODE, 
#         AWLEVEL, 
#         DistanceED)


#just this part
write.table(x = partB, sep=",", 
            file= paste0(path, "Completions_PartB.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

#append to the upload doc
write.table(x = partB, sep=",", 
            file=paste0(path, "Completions_PartsAll.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)


#########################

## Part C -- counts of unduplicated students who are completers by race/ethnicity
#want:
#UNITID=nnnnnn,SURVSECT=COM,PART=C,RACE=2,SEX=1,COUNT=nnnnn 
#UNITID=nnnnnn,SURVSECT=COM,PART=C,RACE=2,SEX=2,COUNT=nnnnn 
#UNITID=nnnnnn,SURVSECT=COM,PART=C,RACE=j,SEX=k,COUNT=nnnnn 

#this is counting STUDENTS, not degrees --  requires deduplication


partC <- startingdf %>%
  select(Unitid, StudentId, RaceEthnicity, Sex) %>%
  
  #deduplicate
  unique() %>%
  
  #aggregate and count
  group_by(Unitid, RaceEthnicity, Sex) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  
  #sort for easy viewing
  arrange(RaceEthnicity,
          Sex) %>%
  
  #format for upload
  mutate(UNITID = paste0("UNITID=", Unitid),
         SURVSECT = "SURVSECT=COM",
         PART = "PART=C",
         RACE = paste0("RACE=", RaceEthnicity),
         SEX = paste0("SEX=", Sex),
         COUNT = paste0("COUNT=", Count)) %>% 
  select(UNITID, 
         SURVSECT, 
         PART, 
         RACE, 
         SEX, 
         COUNT) #%>%
# arrange(UNITID, 
#         SURVSECT, 
#         PART, 
#         RACE, 
#         SEX)


#just this part
write.table(x = partC, sep=",", 
            file= paste0(path, "Completions_PartC.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

#append to the upload doc
write.table(x = partC, sep=",", 
            file=paste0(path, "Completions_PartsAll.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)




#########################

## Part D --- count of unique completers at each award level by race/sex/age categories

#want:
#UNITID=nnnnnn,SURVSECT=COM,PART=D,CTLEVEL=3,CRACE15=nnnnn,CRACE16=nnnnn,CRACE41=nnnnn...

#need to include award levels that have no completers

#check extracips list for award levels not included in the startingdf
extralevel_D <- extracips %>% 
  select(Unitid,
         DegreeLevel) %>% 
  unique() %>%
  filter(!(DegreeLevel %in% startingdf$DegreeLevel)) %>%
  
  #add dummy data to any award levels found
  mutate(StudentId = dummy_studentid,
         RaceEthnicity = 1, 
         Sex = 1, 
         Birthdate = lubridate::ymd("1900-01-01"),
         CountRE = 0,
         CountSex = 0,
         CountAge = 0) %>%
  
  #reorder for rbind
  select(Unitid, 
         StudentId, 
         everything())


#set up an df with 0-rows to ensure we get all 
#race/ethnicity, sex, and age categories in the final output
dummy_demographics <- data.frame(Unitid = ipeds_unitid, 
                                 StudentId = dummy_studentid, 
                                 DegreeLevel = max(startingdf$DegreeLevel), 
                                 RaceEthnicity = c(1:9),
                                 Sex = c(1, 1, 1, 1, 1, 2, 2, 2, 2), 
                                 Age = c(15, 20, 25, 30, 35, 40, 45, 50, NA),
                                 CountRE = 0,
                                 CountSex = 0, 
                                 CountAge = 0)




partD <- startingdf %>%
  select(Unitid, StudentId, DegreeLevel, RaceEthnicity, Sex, Age) %>%
  
  #add values which will be summed later
  mutate(CountRE = 1, CountSex = 1, CountAge = 1) %>%
  
  #add any extra award levels
  rbind(extralevel_D) %>%
  
  #add dummy demographics to make sure the spread works correctly later
  rbind(dummy_demographics) %>%
  
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
                                .default = "ZRACEETH")) %>%
  spread(key = RaceEthnicity, value = CountRE) %>%
  
  #recode and spread Sex to get IPEDS columns
  mutate(Sex = recode(Sex,
                      `1` = "CRACE15",
                      `2` = "CRACE16",
                      .default = "ZRACESEX")) %>%
  spread(key = Sex, value = CountSex) %>%
  
  #recode and spread Age to get IPEDS columns
  mutate(AgeGroup = ifelse(floor(Age) < 18, "AGE1",
                           ifelse(floor(Age) <= 24, "AGE2",
                                  ifelse(floor(Age) <= 39, "AGE3",
                                         ifelse(floor(Age) >= 40, "AGE4",
                                                "AGE9"))))) %>%
  mutate(AgeGroup = ifelse(is.na(Age), 
                           "AGE5", 
                           AgeGroup)) %>%
  
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
         CRACE15, CRACE16, 
         CRACE17, CRACE41, CRACE42, CRACE43, 
         CRACE44, CRACE45, CRACE46, CRACE47, CRACE23, 
         AGE1, AGE2, AGE3, AGE4, AGE5 ) #%>%
# arrange(UNITID, SURVSECT, PART, CTLEVEL)



#just this part
write.table(x = partD, sep=",", 
            file= paste0(path, "Completions_PartD.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE)

#append to the upload doc
write.table(x = partD, sep=",", 
            file=paste0(path, "Completions_PartsAll.txt"),
            quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)



############
## Warnings from recoding failures

#Award Level
if(("CTLEVEL=9" %in% partD$CTLEVEL) != 0) {
  svDialogs::dlg_message("Warning! Your Part D results contain unknown values for degree level. 
                         Please check your data and rerun from the top.")
}

#RaceEthnicity 
if(("ZRACEETH" %in% colnames(partD)) != 0){
  svDialogs::dlg_message("Warning!  Your results contain unknown values for race/ethnicity. 
                         Please check your data and rerun from the top.")
}

#Sex
if(("ZRACESEX" %in% colnames(partD)) != 0){
  svDialogs::dlg_message("Warning!  Your results contain unknown values for sex. 
                         Please check your data and rerun from the top.")
}


#Age
if(("AGE9" %in% colnames(partD)) != 0){
  svDialogs::dlg_message("Warning!  Your results contain unknown values for age. 
                         Please check your data and rerun from the top.")
}


#################
## Status message: finished

svDialogs::dlg_message(paste0("Completions file available. Please see results at ", path))