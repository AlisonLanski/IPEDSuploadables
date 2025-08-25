#' Make Completions Part D
#'
#' @param df A dataframe of student/degree information
#' @param extracips A dataframe of cips offered by the institution but not in \code{'df'}
#'
#' @importFrom rlang .data
#' @importFrom dplyr select filter mutate bind_rows group_by ungroup summarize arrange everything recode distinct
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_com_part_D <- function(df, extracips = NULL) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  if(!is.null(extracips)) {
    colnames(extracips) <- stringr::str_to_upper(colnames(extracips))

    #check extracips list for award levels not included in the startingdf
    extralevel_D <- extracips %>%
                    dplyr::select("UNITID",
                                  "DEGREELEVEL") %>%
                    dplyr::distinct() %>%
                    dplyr::filter(!(.data$DEGREELEVEL %in% df$DEGREELEVEL)) %>%
                    #add dummy data to any award levels found
                    dplyr::mutate(STUDENTID = "dummy_studentid",
                                  RACEETHNICITY = 1,
                                  SEX = 1,
                                  BIRTHDATE = lubridate::ymd("1900-01-01"),
                                  COUNTRE = 0,
                                  COUNTSEX = 0,
                                  COUNTAGE = 0
                    ) %>%
                    #reorder for rbind
                    dplyr::select("UNITID",
                                  "STUDENTID",
                                  dplyr::everything())
  } else {
    extralevel_D <- data.frame(UNITID = df$UNITID[1],
                               STUDENTID = "dummy_studentid",
                               DEGREELEVEL = df$DEGREELEVEL[1],
                               RACEETHNICITY = 1,
                               SEX = 1,
                               BIRTHDATE = lubridate::ymd("1900-01-01"),
                               COUNTRE = 0,
                               COUNTSEX = 0,
                               COUNTAGE = 0,
                               stringsAsFactors = FALSE)
  }

  #set up an df with 0-rows to ensure we get all
  #race/ethnicity, sex, and age categories in the final output

  ipeds_unitid <- as.character(get_ipeds_unitid(df))

  dummy_demographics <- data.frame(UNITID = ipeds_unitid,
                                   STUDENTID = "dummy_studentid",
                                   DEGREELEVEL = max(df$DEGREELEVEL),
                                   RACEETHNICITY = c(1:9),
                                   SEX = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
                                   AGE = c(15, 20, 25, 30, 35, 40, 45, 50, NA),
                                   COUNTRE = 0,
                                   COUNTSEX = 0,
                                   COUNTAGE = 0,
                                   stringsAsFactors = FALSE)

  partD <- df %>%
    dplyr::select("UNITID",
                  "STUDENTID",
                  "DEGREELEVEL",
                  "RACEETHNICITY",
                  "SEX",
                  "AGE") %>%
    dplyr::filter(.data$SEX %in% c(1, 2)) %>%
    #add values which will be summed later
    dplyr::mutate(COUNTRE = 1,
                  COUNTSEX = 1,
                  COUNTAGE = 1
    ) %>%
    #add any extra award levels
    dplyr::bind_rows(extralevel_D) %>%
    #add dummy demographics to make sure the spread works correctly later
    dplyr::bind_rows(dummy_demographics) %>%
    #recode before removing duplicates per student
    dplyr::mutate(CTLEVEL = dplyr::recode(.data$DEGREELEVEL,
                                          "1a" = 8,
                                          "1b" = 9,
                                          "2" = 2,
                                          "3" = 3,
                                          "4" = 2,
                                          "5" = 4,
                                          "6" = 7,
                                          "7" = 5,
                                          "8" = 7,
                                          "17" = 6,
                                          "18" = 6,
                                          "19" = 6,
                                          .default = 99)
    ) %>%
    dplyr::select(-"DEGREELEVEL") %>%
    #one row per student per level per unitid (keep RE/Sex/Birthdate)
    dplyr::distinct() %>%
    #recode and spread RaceEthnicity to get IPEDS columns
    dplyr::mutate(RACEETHNICITY = recode(.data$RACEETHNICITY,
                                         `1` = "CRACE17",
                                         `2` = "CRACE41",
                                         `3` = "CRACE42",
                                         `4` = "CRACE43",
                                         `5` = "CRACE44",
                                         `6` = "CRACE45",
                                         `7` = "CRACE46",
                                         `8` = "CRACE47",
                                         `9` = "CRACE23",
                                         .default = "ZRACEETH")
    ) %>%
    tidyr::pivot_wider(names_from = "RACEETHNICITY", values_from = "COUNTRE") %>%
    #recode and spread Sex to get IPEDS columns
    dplyr::mutate(SEX = recode(.data$SEX,
                               `1` = "CRACE15",
                               `2` = "CRACE16",
                               .default = "ZRACESEX")
    ) %>%
    tidyr::pivot_wider(names_from = "SEX", values_from = "COUNTSEX") %>%
    #recode and spread Age to get IPEDS columns
    dplyr::mutate(AgeGroup = case_when(
      floor(.data$AGE) < 18 ~ "AGE1",
      floor(.data$AGE) <= 24 ~ "AGE2",
      floor(.data$AGE) <= 39 ~ "AGE3",
      floor(.data$AGE) >= 40 ~ "AGE4",
      is.na(.data$AGE) ~ "AGE5",
      TRUE ~ "AGE9"
    )
    ) %>%
    tidyr::pivot_wider(names_from = "AgeGroup", values_from = "COUNTAGE") %>%
    #aggregate and add counts in spread columns;
    #extra award levels and dummy demographics have values of 0
    dplyr::group_by(.data$UNITID, .data$CTLEVEL) %>%
    dplyr::summarize(CRACE15 = sum(.data$CRACE15, na.rm = T),
                     CRACE16 = sum(.data$CRACE16, na.rm = T),
                     CRACE17 = sum(.data$CRACE17, na.rm = T),
                     CRACE41 = sum(.data$CRACE41, na.rm = T),
                     CRACE42 = sum(.data$CRACE42, na.rm = T),
                     CRACE43 = sum(.data$CRACE43, na.rm = T),
                     CRACE44 = sum(.data$CRACE44, na.rm = T),
                     CRACE45 = sum(.data$CRACE45, na.rm = T),
                     CRACE46 = sum(.data$CRACE46, na.rm = T),
                     CRACE47 = sum(.data$CRACE47, na.rm = T),
                     CRACE23 = sum(.data$CRACE23, na.rm = T),
                     AGE1 = sum(.data$AGE1, na.rm = T),
                     AGE2 = sum(.data$AGE2, na.rm = T),
                     AGE3 = sum(.data$AGE3, na.rm = T),
                     AGE4 = sum(.data$AGE4, na.rm = T),
                     AGE5 = sum(.data$AGE5, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    #sort for easier viewing
    dplyr::arrange(.data$CTLEVEL) %>%
    #format for upload
    dplyr::transmute(UNITID = .data$UNITID,
                     SURVSECT = "COM",
                     PART = "D",
                     CTLEVEL = .data$CTLEVEL,
                     CRACE15 = .data$CRACE15,
                     CRACE16 = .data$CRACE16,
                     CRACE17 = .data$CRACE17,
                     CRACE41 = .data$CRACE41,
                     CRACE42 = .data$CRACE42,
                     CRACE43 = .data$CRACE43,
                     CRACE44 = .data$CRACE44,
                     CRACE45 = .data$CRACE45,
                     CRACE46 = .data$CRACE46,
                     CRACE47 = .data$CRACE47,
                     CRACE23 = .data$CRACE23,
                     AGE1 = .data$AGE1,
                     AGE2 = .data$AGE2,
                     AGE3 = .data$AGE3,
                     AGE4 = .data$AGE4,
                     AGE5 = .data$AGE5
    )

}
