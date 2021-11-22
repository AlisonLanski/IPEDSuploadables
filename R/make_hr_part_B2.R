#' Produce IPEDS HR Part B2
#'
#' @description  Part B2 --- Full-time non-instructional staff by tenure, medical school, and occupational category
#'
#' @param df a dataframe
#' @param output a string (\code{"part"}, \code{"full"}, or \code{"both"})
#'
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom rlang .data
#' @importFrom stringr str_to_upper
#'
#' @return a txt file
#' @export
#'

make_hr_part_B2 <- function(df, output = "part") {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #set up the grid of options
  combos_B2 <- expand.grid(UNITID = get_ipeds_unitid(df),
                           TENURE = c(1:7),
                           ISMEDICAL = c(0:1),
                           OCCCATEGORY1 = c(2:12),
                           COUNT = 0)

  #produce the uploadable format
  partB2 <- df %>%
            dplyr::filter(.data$CURRENTEMPLOYEE == 1,
                          .data$INSTRUCTIONAL == 0,
                          .data$FTPT == "F",
                          .data$OCCCATEGORY1 %in% c(2:12)) %>%
            dplyr::select(.data$UNITID,
                          .data$TENURE,
                          .data$ISMEDICAL,
                          .data$OCCCATEGORY1,
                          .data$COUNT) %>%
            #add extra combinations
            dplyr::bind_rows(combos_B2) %>%
            #aggregate the full data
            dplyr::group_by(.data$UNITID,
                            .data$TENURE,
                            .data$ISMEDICAL,
                            .data$OCCCATEGORY1) %>%
            dplyr::summarize(COUNT = sum(.data$COUNT)) %>%
            dplyr::ungroup() %>%
            #sort for easy viewing
            dplyr::arrange(.data$TENURE,
                           .data$ISMEDICAL,
                           .data$OCCCATEGORY1) %>%
            #format for upload
            dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                             SURVSECT = "SURVSECT=HR1",
                             PART = "PART=B2",
                             TENURE = paste0("TENURE=", .data$TENURE),
                             ISMEDICAL = paste0("ISMEDICAL=", .data$ISMEDICAL),
                             OCCCATEGORY1 = paste0("OCCCATEGORY1=", .data$OCCCATEGORY1),
                             COUNT = paste0("COUNT=", .data$COUNT))

  #create the txt file
  write_report(df = partB2,
               component = "HumanResources",
               part = "PartB2",
               output = output)
}
