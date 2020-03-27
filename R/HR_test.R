#Test the HR code

#run dummy data
source("R/HumanResources_DummyData.R")

#source the functions
#ignore the pop message; it lies
source("R/functions_HR.R")

#do setup that isn't in a function yet

#load package
library(tidyverse)


#set an output path:
path <- svDialogs::dlg_dir(default = getwd(), title = "Select the file output location")$res

#make sure the final / is in the path (before the filename)
if(!str_detect(path, pattern = "/$")) {
  path <- paste0(path, "/")
}

#set the school's unitid (for later) -- default is the dummy id
ipeds_unitid  <- as.numeric(svDialogs::dlgInput("What is your school's IPEDS Unitid?",111111)$res)


#run the functions

ipeds_df <- prep_hr_data_frame(ipeds_df)
make_hr_part_A1(ipeds_df)
make_hr_part_A2(ipeds_df)
make_hr_part_B1(ipeds_df)
make_hr_part_B2(ipeds_df)
make_hr_part_B3(ipeds_df)
make_hr_part_D1(ipeds_df)
make_hr_part_D2(ipeds_df)
make_hr_part_D3(ipeds_df)
make_hr_part_D4(ipeds_df)
make_hr_part_G1(ipeds_df)
make_hr_part_G2(ipeds_df)
make_hr_part_H1(ipeds_df)
make_hr_part_H2(ipeds_df)
