
#' Produce IPEDS HR Part G1
#'
#' @description  Part G1 --- Salaries of Instructional staff
#'
#' @param df a dataframe
#' @param output a string (\code{"part"}, \code{"full"}, or \code{"both"})
#'
#' @return a txt file
#' @export
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#'




make_hr_part_G1 <- function(df, output = 'part') {

  #set up the grid of options
  combos_G1 <- expand.grid(Unitid = get_ipeds_unitid(df),
                           Rank = c(1:6),
                           Gender = c(1:2),
                           Months = c(8:12),
                           Salary = 0,
                           EmpId = 'dummy_id',
                           Count = 0)

  #produce the uploadable format
  partG1 <- df %>%
    dplyr::filter(.data$CurrentEmployee == 1,
                  .data$Instructional == 1, #instructional
                  .data$FtPt == "F") %>%  #not listed in uploadable instructions -- verify?
    dplyr::select(.data$Unitid,
                  .data$Rank,
                  .data$Gender,
                  .data$Months,
                  .data$Salary,
                  .data$EmpId,
                  .data$Count) %>%
    #add extra combinations
    dplyr::bind_rows(combos_G1) %>%
    #reset rank 7 as rank 6
    dplyr::mutate(Rank = ifelse(.data$Rank == 7, 6, .data$Rank),
           months_count = dplyr::recode(.data$Months,
                                 `12` = '12mCount',
                                 `11` = '11mCount',
                                 `10` = '10mCount',
                                 `9` = '9mCount',
                                 `8` = 'LessThan9mCount',
                                 .default = 'ZzCount'),
           salary_sum = dplyr::recode(.data$Months,
                               `12` = '12mSoutlays',
                               `11` = '11mSoutlays',
                               `10` = '10mSoutlays',
                               `9` = '9mSoutlays',
                               .default = 'ZzSoutlays')
    ) %>%
    tidyr::pivot_wider(names_from = .data$months_count, values_from = .data$Count) %>%
    tidyr::pivot_wider(names_from = .data$salary_sum, values_from = .data$Salary) %>%
    #aggregate the full data
    dplyr::group_by(.data$Unitid, .data$Rank, .data$Gender) %>%
    dplyr::summarize(`12mCount` = sum(.data$`12mCount`, na.rm = T),
              `11mCount` = sum(.data$`11mCount`, na.rm = T),
              `10mCount` = sum(.data$`10mCount`, na.rm = T),
              `9mCount` = sum(.data$`9mCount`, na.rm = T),
              `LessThan9mCount` = sum(.data$`LessThan9mCount`, na.rm = T),
              #`ZzCount` = sum(.data$`ZzCount`, na.rm = T),
              `12mSoutlays` = sum(.data$`12mSoutlays`, na.rm = T),
              `11mSoutlays` = sum(.data$`11mSoutlays`, na.rm = T),
              `10mSoutlays` = sum(.data$`10mSoutlays`, na.rm = T),
              `9mSoutlays` = sum(.data$`9mSoutlays`, na.rm = T),
              `ZzSoutlays` = sum(.data$`ZzSoutlays`, na.rm = T) #this will have values for the LessThan9 folks
    ) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$Rank,
                   .data$Gender) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
              SURVSECT = "SURVSECT=HR1",
              PART = "PART=G1",
              RANK = paste0("RANK=", .data$Rank),
              GENDER = paste0("GENDER=", .data$Gender),
              `12mCount` = paste0("12MCOUNT=", .data$`12mCount`),
              `11mCount` = paste0("11MCOUNT=", .data$`11mCount`),
              `10mCount` = paste0("10MCOUNT=", .data$`10mCount`),
              `9mCount` = paste0("9MCOUNT=", .data$`9mCount`),
              `LessThan9mCount` = paste0("LESSTHAN9MCOUNT=", .data$LessThan9mCount),
              #`ZzCount` = paste0("ZzCount=", .data$ZzCount),
              `12mSoutlays` = paste0("12MSOUTLAYS=", .data$`12mSoutlays`),
              `11mSoutlays` = paste0("11MSOUTLAYS=", .data$`11mSoutlays`),
              `10mSoutlays` = paste0("10MSOUTLAYS=", .data$`10mSoutlays`),
              `9mSoutlays` = paste0("9MSOUTLAYS=", .data$`9mSoutlays`)#,
              #`ZzSoutlays` = paste0("ZzSoutlays=", ZzSoutlays)
    )



  #create the txt file
  write_report(df = partG1,
               component = 'HumanResources',
               part = "PartG1",
               output = output)
}
