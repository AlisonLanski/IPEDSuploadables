#' Some initial recoding for Human Resources
#'
#' @param df a dataframe
#'
#' @importFrom dplyr mutate recode
#' @importFrom rlang .data
#' @importFrom stringr str_to_upper
#'
#' @return a dataframe
#' @export
#'

prep_hr_data_frame <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  ## Warnings for data checks -----

  #Gender
  if(min(df$GENDER) < 1 | max(df$GENDER) > 2) {
    warning(paste0("Check Gender: invalid values found for EmpId: ",
                   toString(df$EMPID[!df$GENDER %in% c(1:2)])))
  }

  #RaceEthnicity
  if(min(df$RACEETHNICITY) < 1 | max(df$RACEETHNICITY) > 9){
    warning(paste0("Check RaceEthnicity: invalid values found for EmpId: ",
                   toString(df$EMPID[!df$RACEETHNICITY %in% c(1:9)])))
  }

  #OccCat
  if(min(df$OCCCATEGORY3) < 1 | max(df$OCCCATEGORY3) > 24 | 21 %in% df$OCCCATEGORY3){
    warning(paste0("Check OccCategory3: invalid values found for EmpId: ",
                   toString(df$EMPID[!df$OCCCATEGORY3 %in% c(1:20, 22:24)])))
  }


  #MonthsWorked
  #MONTHS (for Salary-spread); only matters for current, instructional, ft employees
  if(sum(!(df$MONTHS[df$CURRENTEMPLOYEE == 1 &
                      df$FTPT == 'F' &
                      df$OCCCATEGORY3 %in% c(1:4)] %in%
            c(8, 9, 10, 11, 12))) != 0) {
    warning(paste0("Check Months: invalid values found for EmpId: ",
                   toString(df$EMPID[df$CURRENTEMPLOYEE == 1 &
                                     df$FTPT == 'F' &
                                     df$OCCCATEGORY3 %in% c(1:4) &
                                     !df$MONTHS %in% c(8:12)])))


  }

  #Rule change: all new hires must be current employees=
  if(sum(df$CURRENTEMPLOYEE == 0 & df$NEWHIRE == 1) > 0){
    warning(paste0("All new hires must also be current employees to be reportable; update current status or remove from data EmpId: ",
                   toString(df$EMPID[df$CURRENTEMPLOYEE == 0 & df$NEWHIRE == 1])))
  }

  df <- df %>%
        #add a combined REG column
        dplyr::mutate(REG = ifelse(.data$GENDER == 1,
                                  .data$RACEETHNICITY,
                                    ifelse(.data$GENDER == 2,
                                           .data$RACEETHNICITY + 9,
                                           99)
                                  ),
                      #set up other OccCategories based on Version 3 (complete)
                      OCCCATEGORY1 = dplyr::recode(df$OCCCATEGORY3,
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
                      OCCCATEGORY2 = dplyr::recode(.data$OCCCATEGORY3,
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
                      OCCCATEGORY4 = dplyr::recode(.data$OCCCATEGORY3,
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
                      OCCCATEGORY5 = dplyr::recode(.data$OCCCATEGORY3,
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
                      INSTFUNCTION = dplyr::recode(.data$OCCCATEGORY3,
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
                      INSTRUCTIONAL = dplyr::recode(.data$OCCCATEGORY3,
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
                      #set up default: 1 row = COUNT 1 so we can add later
                      COUNT = 1,
                      #specify EMPID as character to avoid type problems
                      EMPID = as.character(.data$EMPID),
                      #specify UNITID as character to avoid type problems
                      UNITID = as.character(.data$UNITID))


  return(df)
}


