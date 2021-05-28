#' Make Fall Enrollment Part C
#'
#' @param df A dataframe of student/degree information
#' @param extracips A dataframe of cips offered by the institution but not in \code{'df'}
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by summarise ungroup bind_rows arrange transmute n mutate
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'


make_ef1_part_C <- function(df, extracips = NULL, output = "part", format = "both") {

  partC <- df %>%
    dplyr::select(.data$State,
                  .data$HS) %>%
    dplyr::mutate(Line = dplyr::recode(.data$State,
                                       "AL" = "01",
                                       "AK" = "02",
                                       "AZ" = "04",
                                       "AR" = "05",
                                       "CA" = "06",
                                       "CO" = "08",
                                       "CT" = "09",
                                       "DE" = "10",
                                       "DC" = "11",
                                       "FL" = "12",
                                       "GA" = "13",
                                       "HI" = "15",
                                       "ID" = "16",
                                       "IL" = "17",
                                       "IN" = "18",
                                       "IA" = "19",
                                       "KS" = "20",
                                       "KY" = "21",
                                       "LA" = "22",
                                       "ME" = "23",
                                       "MD" = "24",
                                       "MA" = "25",
                                       "MI" = "26",
                                       "MN" = "27",
                                       "MS" = "28",
                                       "MO" = "29",
                                       "MT" = "30",
                                       "NE" = "31",
                                       "NV" = "32",
                                       "NH" = "33",
                                       "NJ" = "34",
                                       "NM" = "35",
                                       "NY" = "36",
                                       "NC" = "37",
                                       "ND" = "38",
                                       "OH" = "39",
                                       "OK" = "40",
                                       "OR" = "41",
                                       "PA" = "42",
                                       "RI" = "44",
                                       "SC" = "45",
                                       "SD" = "46",
                                       "TN" = "47",
                                       "TX" = "48",
                                       "UT" = "49",
                                       "VT" = "50",
                                       "VI" = "51",
                                       "WA" = "53",
                                       "WV" = "54",
                                       "WI" = "55",
                                       "WY" = "56",
                                       "Unknown" = "57",
                                       "American Samoa" = "60",
                                       "Federated States of Micronesia" = "64",
                                       "Guam" = "66",
                                       "Marshall Islands" = "68",
                                       "Northern Marianas" = "69",
                                       "Palau" = "70",
                                       "Puerto Rico" = "72",
                                       "Virgin Islands" = "78",
                                       "Foreign Countries" = "90"
                                       )
                  ) %>%
    dplyr::group_by(.data$Line, .data$HS) %>%
    dplyr::summarise(Count = n()) %>%
    #sort for easy viewing
    dplyr::arrange(.data$Line, .data$HS) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=EF1",
                     PART = "PART=C",
                     LINE = paste0("LINE=", .data$Line),
                     HS = paste0("RACE=", .data$HS),
                     COUNT = paste0("COUNT=", .data$Count)
    )

  #create the txt file
  write_report(df = partC,
               component = "FallEnrollment",
               part = "PartC",
               output = output,
               format = format)
}
