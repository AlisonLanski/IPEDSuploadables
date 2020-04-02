#Test the Completions code

#run dummy data
source("R/CompletionsStartingDf_DummyData.R")

#source the functions
#ignore the pop message; it lies
source("R/functions_Completions.R")

#load package
library(tidyverse)

## Function calls -----
## set paths
path <- set_report_path()

#set the school's unitid (for later)
## PULL OUT
ipeds_unitid  <- svDialogs::dlgInput("What is your school's IPEDS Unitid?", 999999)$res

#set a dummy studentID (for later)
## PRE-SET???
#dummy_studentid <- svDialogs::dlgInput("Provide a value that can be used as a dummy-student ID")$res

df <- prep_com_data_files(df = startingdf)
extracips <- prep_com_data_files(df = extracips)

## make all the files individually
make_com_part_A(df = df, extracips = extracips)
make_com_part_B(df = df, extracips = extracips)
make_com_part_C(df = df)
make_com_part_D(df = df)

## or just make it in one line if you so please
make_completions(df = df, extracips = extracips)
