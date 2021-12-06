#' Produce IPEDS HR Part B3
#'
#' @description  Part B3 --- Full-time non-instructional staff by medical school, and occupational category
#'
#' @param df a dataframe
#' @param output a string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom rlang .data
#' @importFrom stringr str_to_upper
#'
#' @return a txt file
#' @export
#'

make_hr_part_B3 <- function(df, output = "part", format = "both") {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #set up the grid of options
  combos_B3 <- expand.grid(UNITID = get_ipeds_unitid(df),
                           ISMEDICAL = c(0:1),
                           OCCCATEGORY1 = c(13:17),
                           COUNT = 0)

  #produce the uploadable format
  partB3 <- df %>%
            dplyr::filter(.data$CURRENTEMPLOYEE == 1,
                          .data$INSTRUCTIONAL == 0,
                          .data$FTPT == "F",
                          .data$OCCCATEGORY1 %in% c(13:17)) %>%
            dplyr::select(.data$UNITID,
                          .data$ISMEDICAL,
                          .data$OCCCATEGORY1,
                          .data$COUNT) %>%
            #add extra combinations
            dplyr::bind_rows(combos_B3) %>%
            #aggregate the full data
            dplyr::group_by(.data$UNITID,
                            .data$ISMEDICAL,
                            .data$OCCCATEGORY1) %>%
            dplyr::summarize(COUNT = sum(.data$COUNT)) %>%
            dplyr::ungroup() %>%
            #sort for easy viewing
            dplyr::arrange(.data$ISMEDICAL,
                           .data$OCCCATEGORY1) %>%
            #format for upload
            dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                             SURVSECT = "SURVSECT=HR1",
                             PART = "PART=B3",
                             ISMEDICAL = paste0("ISMEDICAL=", .data$ISMEDICAL),
                             OCCCATEGORY1 = paste0("OCCCATEGORY1=", .data$OCCCATEGORY1),
                             COUNT = paste0("COUNT=", .data$COUNT))

  #create the txt file
  write_report(df = partB3,
               component = "HumanResources",
               part = "PartB3",
               output = output,
               format = format)
}