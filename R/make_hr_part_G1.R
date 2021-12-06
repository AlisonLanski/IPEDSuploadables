#' Produce IPEDS HR Part G1
#'
#' @description  Part G1 --- Salaries of INSTRUCTIONAL staff
#'
#' @param df a dataframe
#' @param output a string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom stringr str_to_upper
#'
#' @return a txt file
#' @export
#'

make_hr_part_G1 <- function(df, output = "part", format = "both") {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #set up the grid of options
  combos_G1 <- expand.grid(UNITID = get_ipeds_unitid(df),
                           RANK = c(1:6),
                           GENDER = c(1:2),
                           MONTHS = c(8:12),
                           SALARY = 0,
                           EMPID = "dummy_id",
                           COUNT = 0)

  #produce the uploadable format
  partG1 <- df %>%
            dplyr::filter(.data$CURRENTEMPLOYEE == 1,
                          .data$INSTRUCTIONAL == 1, #instructional
                          .data$FTPT == "F") %>%  #not listed in uploadable instructions -- verify?
            dplyr::select(.data$UNITID,
                          .data$RANK,
                          .data$GENDER,
                          .data$MONTHS,
                          .data$SALARY,
                          .data$EMPID,
                          .data$COUNT) %>%
            #add extra combinations
            dplyr::bind_rows(combos_G1) %>%
            #reset rank 7 as rank 6
            dplyr::mutate(RANK = ifelse(.data$RANK == 7, 6, .data$RANK),
                          months_count = dplyr::recode(.data$MONTHS,
                                                       `12` = "12mCOUNT",
                                                       `11` = "11mCOUNT",
                                                       `10` = "10mCOUNT",
                                                       `9` = "9mCOUNT",
                                                       `8` = "LessThan9mCOUNT",
                                                       .default = "ZzCOUNT"),
                          salary_sum = dplyr::recode(.data$MONTHS,
                                                     `12` = "12mSoutlays",
                                                     `11` = "11mSoutlays",
                                                     `10` = "10mSoutlays",
                                                     `9` = "9mSoutlays",
                                                     .default = "ZzSoutlays")
            ) %>%
            tidyr::pivot_wider(names_from = .data$months_count,
                               values_from = .data$COUNT) %>%
            tidyr::pivot_wider(names_from = .data$salary_sum,
                               values_from = .data$SALARY) %>%
            #aggregate the full data
            dplyr::group_by(.data$UNITID, .data$RANK, .data$GENDER) %>%
            dplyr::summarize(`12mCOUNT` = sum(.data$`12mCOUNT`, na.rm = T),
                             `11mCOUNT` = sum(.data$`11mCOUNT`, na.rm = T),
                             `10mCOUNT` = sum(.data$`10mCOUNT`, na.rm = T),
                             `9mCOUNT` = sum(.data$`9mCOUNT`, na.rm = T),
                             `LessThan9mCOUNT` = sum(.data$`LessThan9mCOUNT`, na.rm = T),
                             #`ZzCOUNT` = sum(.data$`ZzCOUNT`, na.rm = T),
                             `12mSoutlays` = sum(.data$`12mSoutlays`, na.rm = T),
                             `11mSoutlays` = sum(.data$`11mSoutlays`, na.rm = T),
                             `10mSoutlays` = sum(.data$`10mSoutlays`, na.rm = T),
                             `9mSoutlays` = sum(.data$`9mSoutlays`, na.rm = T),
                             `ZzSoutlays` = sum(.data$`ZzSoutlays`, na.rm = T)) %>% #this will have values for the LessThan9 folks
            dplyr::ungroup() %>%
            #sort for easy viewing
            dplyr::arrange(.data$RANK,
                           .data$GENDER) %>%
            #format for upload
            dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                             SURVSECT = "SURVSECT=HR1",
                             PART = "PART=G1",
                             RANK = paste0("RANK=", .data$RANK),
                             GENDER = paste0("GENDER=", .data$GENDER),
                             `12mCOUNT` = paste0("12MCOUNT=", .data$`12mCOUNT`),
                             `11mCOUNT` = paste0("11MCOUNT=", .data$`11mCOUNT`),
                             `10mCOUNT` = paste0("10MCOUNT=", .data$`10mCOUNT`),
                             `9mCOUNT` = paste0("9MCOUNT=", .data$`9mCOUNT`),
                             `LessThan9mCOUNT` = paste0("LESSTHAN9MCOUNT=", .data$LessThan9mCOUNT),
                             #`ZzCOUNT` = paste0("ZzCOUNT=", .data$ZzCOUNT),
                             `12mSoutlays` = paste0("12MSOUTLAYS=", .data$`12mSoutlays`),
                             `11mSoutlays` = paste0("11MSOUTLAYS=", .data$`11mSoutlays`),
                             `10mSoutlays` = paste0("10MSOUTLAYS=", .data$`10mSoutlays`),
                             `9mSoutlays` = paste0("9MSOUTLAYS=", .data$`9mSoutlays`)#,
                             #`ZzSoutlays` = paste0("ZzSoutlays=", ZzSoutlays)
                             )

  #create the txt file
  write_report(df = partG1,
               component = "HumanResources",
               part = "PartG1",
               output = output,
               format = format)
}
