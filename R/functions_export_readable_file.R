##################################################
# A series of functions to get readable exports  #
# to compare against the uploaded file in IPEDS. #
##################################################

## Load packages -----
library(tidyverse) 

## Function: Export Part A -----
export_part_A <- function(partA) {
  
  df <- list()
  for (i in 1:nrow(partA)) {
    df$UNITID[i] <- str_split(partA$UNITID, "=")[[i]][2]
    df$SURVSECT[i] <- str_split(partA$SURVSECT, "=")[[i]][2]
    df$PART[i] <- str_split(partA$PART, "=")[[i]][2]
    df$MAJORNUM[i] <- str_split(partA$MAJORNUM, "=")[[i]][2]
    df$CIPCODE[i] <- str_split(partA$CIPCODE, "=")[[i]][2]
    df$AWLEVEL[i] <- str_split(partA$AWLEVEL, "=")[[i]][2]
    df$RACE[i] <- str_split(partA$RACE, "=")[[i]][2]
    df$SEX[i] <- str_split(partA$SEX, "=")[[i]][2]
    df$COUNT[i] <- str_split(partA$COUNT, "=")[[i]][2]
  }
  
  df <- as.data.frame(df) %>%
        select(-c(UNITID, SURVSECT, PART)) %>% 
        mutate(COUNT = as.numeric(as.character(COUNT))) %>%
        spread(key = RACE, value = COUNT) %>%
        rename("NONRS" = "1",
               "HISPA" = "2",
               "AIAKN" = "3",
               "ASIAN" = "4",
               "BLACK" = "5",
               "PACIF" = "6",
               "WHITE" = "7",
               "MULTI" = "8",
               "UNKWN" = "9") %>%
        mutate(NONRS = replace_na(NONRS, 0),
               HISPA = replace_na(HISPA, 0),
               AIAKN = replace_na(AIAKN, 0),
               ASIAN = replace_na(ASIAN, 0),
               BLACK = replace_na(BLACK, 0),
               PACIF = replace_na(PACIF, 0),
               WHITE = replace_na(WHITE, 0),
               MULTI = replace_na(MULTI, 0),
               UNKWN = replace_na(UNKWN, 0)) %>%
        group_by(MAJORNUM, CIPCODE, AWLEVEL, SEX) %>%
        mutate(TOTAL = sum(NONRS, HISPA, AIAKN, ASIAN, 
                           BLACK, PACIF, WHITE, MULTI, UNKWN))
  
  write.csv(df, here("Completions_PartA_for_Comparison.csv"))
}

## Function: Export Part B -----
export_part_B <- function(partB) {
  
  df <- list()
  for (i in 1:nrow(partB)) {
    df$UNITID[i] <- str_split(partB$UNITID, "=")[[i]][2]
    df$SURVSECT[i] <- str_split(partB$SURVSECT, "=")[[i]][2]
    df$PART[i] <- str_split(partB$PART, "=")[[i]][2]
    df$MAJORNUM[i] <- str_split(partB$MAJORNUM, "=")[[i]][2]
    df$CIPCODE[i] <- str_split(partB$CIPCODE, "=")[[i]][2]
    df$AWLEVEL[i] <- str_split(partB$AWLEVEL, "=")[[i]][2]
    df$DistanceED[i] <- str_split(partB$DistanceED, "=")[[i]][2]
  }
  
  df <- as.data.frame(df)
  
  write.csv(df, here("Completions_PartB_for_Comparison.csv"))
}


## Function: Export Part C -----
export_part_C <- function(partC) {
  
  df <- list()
  for (i in 1:nrow(partC)) {
    df$UNITID[i] <- str_split(partC$UNITID, "=")[[i]][2]
    df$SURVSECT[i] <- str_split(partC$SURVSECT, "=")[[i]][2]
    df$PART[i] <- str_split(partC$PART, "=")[[i]][2]
    df$RACE[i] <- str_split(partC$RACE, "=")[[i]][2]
    df$SEX[i] <- str_split(partC$SEX, "=")[[i]][2]
    df$COUNT[i] <- str_split(partC$COUNT, "=")[[i]][2]
  }
  
  df <- as.data.frame(df) %>%
    select(-c(UNITID, SURVSECT, PART)) %>% 
    mutate(COUNT = as.numeric(as.character(COUNT))) %>%
    spread(key = RACE, value = COUNT) %>%
    rename("NONRS" = "1",
           "HISPA" = "2",
           "AIAKN" = "3",
           "ASIAN" = "4",
           "BLACK" = "5",
           "PACIF" = "6",
           "WHITE" = "7",
           "MULTI" = "8",
           "UNKWN" = "9") %>%
    mutate(NONRS = replace_na(NONRS, 0),
           HISPA = replace_na(HISPA, 0),
           AIAKN = replace_na(AIAKN, 0),
           ASIAN = replace_na(ASIAN, 0),
           BLACK = replace_na(BLACK, 0),
           PACIF = replace_na(PACIF, 0),
           WHITE = replace_na(WHITE, 0),
           MULTI = replace_na(MULTI, 0),
           UNKWN = replace_na(UNKWN, 0)) %>%
    group_by(SEX) %>%
    mutate(TOTAL = sum(NONRS, HISPA, AIAKN, ASIAN, 
                       BLACK, PACIF, WHITE, MULTI, UNKWN))
  
  write.csv(df, here("Completions_PartC_for_Comparison.csv"))
}

## Function: Export Part D -----
export_part_D <- function(partD) {

  df <- list()
  for (i in 1:nrow(partD)) {
    df$UNITID[i] <- str_split(partD$UNITID, "=")[[i]][2]
    df$SURVSECT[i] <- str_split(partD$SURVSECT, "=")[[i]][2]
    df$PART[i] <- str_split(partD$PART, "=")[[i]][2]
    df$CTLEVEL[i] <- str_split(partD$CTLEVEL, "=")[[i]][2]
    df$CRACE15[i] <- str_split(partD$CRACE15, "=")[[i]][2]
    df$CRACE16[i] <- str_split(partD$CRACE16, "=")[[i]][2]
    df$CRACE17[i] <- str_split(partD$CRACE17, "=")[[i]][2]
    df$CRACE41[i] <- str_split(partD$CRACE41, "=")[[i]][2]
    df$CRACE42[i] <- str_split(partD$CRACE42, "=")[[i]][2]
    df$CRACE43[i] <- str_split(partD$CRACE43, "=")[[i]][2]
    df$CRACE44[i] <- str_split(partD$CRACE44, "=")[[i]][2]
    df$CRACE45[i] <- str_split(partD$CRACE45, "=")[[i]][2]
    df$CRACE46[i] <- str_split(partD$CRACE46, "=")[[i]][2]
    df$CRACE47[i] <- str_split(partD$CRACE47, "=")[[i]][2]
    df$CRACE23[i] <- str_split(partD$CRACE23, "=")[[i]][2]
    df$AGE1[i] <- str_split(partD$AGE1, "=")[[i]][2]
    df$AGE2[i] <- str_split(partD$AGE2, "=")[[i]][2]
    df$AGE3[i] <- str_split(partD$AGE3, "=")[[i]][2]
    df$AGE4[i] <- str_split(partD$AGE4, "=")[[i]][2]
    df$AGE5[i] <- str_split(partD$AGE5, "=")[[i]][2]
  }
  

  df <- as.data.frame(df) %>%
    select(-c(UNITID, SURVSECT, PART)) %>% 
    rename("NONRS" = "CRACE17",
           "HISPA" = "CRACE41",
           "AIAKN" = "CRACE42",
           "ASIAN" = "CRACE43",
           "BLACK" = "CRACE44",
           "PACIF" = "CRACE45",
           "WHITE" = "CRACE46",
           "MULTI" = "CRACE47",
           "UNKWN" = "CRACE23") %>%
    mutate(NONRS = replace_na(NONRS, 0),
           HISPA = replace_na(HISPA, 0),
           AIAKN = replace_na(AIAKN, 0),
           ASIAN = replace_na(ASIAN, 0),
           BLACK = replace_na(BLACK, 0),
           PACIF = replace_na(PACIF, 0),
           WHITE = replace_na(WHITE, 0),
           MULTI = replace_na(MULTI, 0),
           UNKWN = replace_na(UNKWN, 0)) %>%
    mutate(TOTAL = sum(NONRS, HISPA, AIAKN, ASIAN, 
                       BLACK, PACIF, WHITE, MULTI, UNKWN))
  
  write.csv(df, here("Completions_PartD_for_Comparison.csv"))
}