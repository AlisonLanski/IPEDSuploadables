#' Make Human Resources Part G1
#'
#' @description  Part G1 --- Salaries of INSTRUCTIONAL staff
#'
#' @param df a dataframe
#'
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom stringr str_to_upper
#'
#' @return a txt file
#' @export
#'

make_hr_part_G1 <- function(df) {

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
                          .data$ISMEDICAL == 0, #non-medical
                          .data$FTPT == "F") %>%
            dplyr::select("UNITID",
                          "RANK",
                          "GENDER",
                          "MONTHS",
                          "SALARY",
                          "EMPID",
                          "COUNT") %>%
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
            tidyr::pivot_wider(names_from = "months_count",
                               values_from = "COUNT") %>%
            tidyr::pivot_wider(names_from = "salary_sum",
                               values_from = "SALARY") %>%
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
            dplyr::transmute(UNITID = .data$UNITID,
                             SURVSECT = "HR1",
                             PART = "G1",
                             RANK = .data$RANK,
                             GENDER = .data$GENDER,
                             `12MCOUNT` =  .data$`12mCOUNT`,
                             `11MCOUNT` = .data$`11mCOUNT`,
                             `10MCOUNT` = .data$`10mCOUNT`,
                             `9MCOUNT` = .data$`9mCOUNT`,
                             `LESSTHAN9MCOUNT` = .data$LessThan9mCOUNT,
                             #dont report the ZZ to ipeds
                             #`ZzCOUNT` =  .data$ZzCOUNT,
                             `12MSOUTLAYS` = .data$`12mSoutlays`,
                             `11MSOUTLAYS` = .data$`11mSoutlays`,
                             `10MSOUTLAYS` = .data$`10mSoutlays`,
                             `9MSOUTLAYS` = .data$`9mSoutlays`#,
                             #`ZzSoutlays` = .data$ZzSoutlays
                             )

}
