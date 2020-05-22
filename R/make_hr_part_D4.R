
#' Produce IPEDS HR Part D4
#'
#' @description  Part D4 --- Part-time Non-instructional staff by medical school, and occupational category
#'
#' @param df a dataframe
#' @param output a string (\code{"part"}, \code{"full"}, or \code{"both"})
#'
#' @return a txt file
#' @export
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom rlang .data
#'




make_hr_part_D4 <- function(df, output = 'part') {

  #set up the grid of options
  combos_D4 <- expand.grid(Unitid = ipeds_unitid,
                           IsMedical = c(0:1),
                           OccCategory3 = c(16:20, 22:24),
                           Count = 0)

  #produce the uploadable format
  partD4 <- df %>%
    dplyr::filter(.data$CurrentEmployee == 1,
                  .data$FtPt == "P",
                  .data$OccCategory3 %in% c(16:20, 22:24)) %>%
    dplyr::select(.data$Unitid,
                  .data$IsMedical,
                  .data$OccCategory3,
                  .data$Count) %>%
    #add extra combinations
    dplyr::bind_rows(combos_D4) %>%
    #aggregate the full data
    dplyr::group_by(.data$Unitid,
                    .data$IsMedical,
                    .data$OccCategory3) %>%
    dplyr::summarize(Count = sum(.data$Count)) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$IsMedical,
                   .data$OccCategory3) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
              SURVSECT = "SURVSECT=HR1",
              PART = "PART=D4",
              ISMEDICAL = paste0("ISMEDICAL=", .data$IsMedical),
              OCCCATEGORY3 = paste0("OCCCATEGORY3=", .data$OccCategory3),
              COUNT = paste0("COUNT=", .data$Count))



  #create the txt file
  write_report(df = partD4,
               component = 'HumanResources',
               path = path,
               part = "PartD4",
               output = output,
               append = FALSE)
}
