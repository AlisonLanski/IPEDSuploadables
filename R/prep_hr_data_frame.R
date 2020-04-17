#' Prepare dataframe for IPEDS HR script
#'
#' @param df a dataframe
#'
#' @return a dataframe
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom dplyr mutate recode
#'

prep_hr_data_frame <- function(df) {

  df <- df %>%
    #add a combined REG column
    mutate(REG = ifelse(
      df$Gender == 1,
      df$RaceEthnicity,
      ifelse(df$Gender == 2,
             df$RaceEthnicity + 9,
             99)
    ),
    #set up other OccCategories based on Version 3 (complete)
    OccCategory1 = recode(df$OccCategory3,
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
    OccCategory2 = recode(df$OccCategory3,
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
    OccCategory4 = recode(df$OccCategory3,
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
    OccCategory5 = recode(df$OccCategory3,
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
    InstFunction = recode(df$OccCategory3,
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
    Instructional = recode(df$OccCategory3,
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
    EmpId = as.character(df$EmpId))

  ## Warnings from recoding failures  -----

  #RaceEthnicityGender
  if(99 %in% df$REG) {
    print("Warning! Some RaceEthnicityGender combinations have failed.
                         Please check your Gender and RaceEthnicity values, then rerun from the top.")
  }

  #OccCats
  if(99 %in% c(df$OccCategory1, df$OccCategory2, df$OccCategory4, df$OccCategory5)){
    print("Warning!  Some Occupational Category recoding has failed.
                         Please check your OccCategory3 values, then rerun from the top.")
  }

  #Months (for Salary-spread); only matters for current employees
  if(sum(!(df$Months[df$CurrentEmployee == 1] %in% c(8, 9, 10, 11, 12, 99))) != 0){
    print("Warning!  Some Months values are not allowed and will break the Salary calculation in G1.
                         Please check your data to ensure use of 8, 9, 10, 11, 12, and 99 only, then rerun from the top.")
  }

  return(df)
}


