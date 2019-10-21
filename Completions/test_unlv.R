## Load packages -----
library(here)

## Source files -----
source(here("Shiloh/R/utilities_standard_header.R"))

## Get data for calculations and prep it -----
## Completions
startingdf <- get_query_from_file(path = here("Shiloh/SQL/"),
                                  file_name = "ipeds_completions_2019-08-30") %>% 
              mutate(BIRTHDATE = as.Date(substr(BIRTHDATE, 1, 10)),
                     MAJORNUMBER = as.numeric(MAJORNUMBER),
                     DEGREELEVEL = replace(DEGREELEVEL, DEGREELEVEL == "13", "5"),
                     DEGREELEVEL = replace(DEGREELEVEL, DEGREELEVEL == "14", "6"),
                     DEGREELEVEL = replace(DEGREELEVEL, DEGREELEVEL == "17", "7"),
                     DEGREELEVEL = replace(DEGREELEVEL, DEGREELEVEL == "18", "8"),
                     DEGREELEVEL = replace(DEGREELEVEL, DEGREELEVEL == "21", "17"),
                     RACEETHNICITY = replace(RACEETHNICITY, RACEETHNICITY == "NONRS", "1"),
                     RACEETHNICITY = replace(RACEETHNICITY, RACEETHNICITY == "HISPA", "2"),
                     RACEETHNICITY = replace(RACEETHNICITY, RACEETHNICITY == "AIAKN", "3"),
                     RACEETHNICITY = replace(RACEETHNICITY, RACEETHNICITY == "ASIAN", "4"),
                     RACEETHNICITY = replace(RACEETHNICITY, RACEETHNICITY == "BLACK", "5"),
                     RACEETHNICITY = replace(RACEETHNICITY, RACEETHNICITY == "PACIF", "6"),
                     RACEETHNICITY = replace(RACEETHNICITY, RACEETHNICITY == "WHITE", "7"),
                     RACEETHNICITY = replace(RACEETHNICITY, RACEETHNICITY == "MULTI", "8"),
                     RACEETHNICITY = replace(RACEETHNICITY, RACEETHNICITY == "UNKWN", "9"),
                     SEX = replace(SEX, SEX == "M", "1"),
                     SEX = replace(SEX, SEX == "F", "2")) %>%
              rename("Unitid" = "UNITID", 
                     "StudentId" = "STUDENTID",
                     "RaceEthnicity" = "RACEETHNICITY",
                     "Sex" = "SEX",
                     "DegreeLevel" = "DEGREELEVEL",
                     "MajorNumber" = "MAJORNUMBER",
                     "MajorCip" = "MAJORCIP",
                     "DistanceEd" = "DISTANCEED",
                     "Birthdate" = "BIRTHDATE" )

extracips <- data.frame(Unitid = 182281, 
                           MajorNumber = 1,
                           MajorCip = 52.1501,
                           DegreeLevel = "5",
                           DistanceEd = 2,
                           RaceEthnicity = 1,
                           Sex = 1,
                           Count = 0) 

extracips <- dbGetQuery(db_con, "select distinct acad_plan, cip_code, degree 
                                from ps_acad_plan_tbl 
                                where eff_status = 'A' 
                                and degree not in ('MINOR','MED','OTD')
                                and acad_plan_type <> 'MIN'
                                and cip_code <> '-'") %>%
          anti_join(startingdf, by = c("CIP_CODE" = "MajorCip")) #%>%
          transmute(Unitid = 182281, 
                    MajorNumber = 1,
                    MajorCip = CIP_CODE,
                    DegreeLevel = case_when(
                      DEGREE %in% () ~ 5,
                      DEGREE %in% () ~ 6,
                      DEGREE %in% () ~ 7,
                      DEGREE %in% () ~ 17
                    ),
                    DistanceEd = 2,
                    RaceEthnicity = 1,
                    Sex = 1,
                    Count = 0)
        
