##
#' Produce IPEDS HR Part B3
#'
#' @description  Part B3 --- Full-time non-instructional staff by medical school, and occupational category
#'
#' @param df a dataframe
#' @param output a string (\code{"part"}, \code{"full"}, or \code{"both"})
#'
#' @return a txt file
#' @export
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom rlang .data
#'


make_hr_part_B3 <- function(df, output = 'part') {

  #set up the grid of options
  combos_B3 <- expand.grid(Unitid = get_ipeds_unitid(df),
                           IsMedical = c(0:1),
                           OccCategory1 = c(13:17),
                           Count = 0)

  #produce the uploadable format
  partB3 <- df %>%
    dplyr::filter(.data$CurrentEmployee == 1,
                  .data$Instructional == 0,
                  .data$FtPt == 'F',
                  .data$OccCategory1 %in% c(13:17)) %>%
    dplyr::select(.data$Unitid,
                  .data$IsMedical,
                  .data$OccCategory1,
                  .data$Count) %>%
    #add extra combinations
    dplyr::bind_rows(combos_B3) %>%
    #aggregate the full data
    dplyr::group_by(.data$Unitid,
                    .data$IsMedical,
                    .data$OccCategory1) %>%
    dplyr::summarize(Count = sum(.data$Count)) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$IsMedical,
                   .data$OccCategory1) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
              SURVSECT = "SURVSECT=HR1",
              PART = "PART=B3",
              ISMEDICAL = paste0("ISMEDICAL=", .data$IsMedical),
              OCCCATEGORY1 = paste0("OCCCATEGORY1=", .data$OccCategory1),
              COUNT = paste0("COUNT=", .data$Count))

  #create the txt file
  write_report(df = partB3,
               component = 'HumanResources',
               part = "PartB3",
               output = output)
}
