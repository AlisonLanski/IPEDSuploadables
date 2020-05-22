
#' Produce IPEDS HR Part H2
#'
#' @description  Part H2 --- New hires by occupational category, Race/Ethnicity/Gender
#'
#' @param df a dataframe
#' @param output a string (\code{"part"}, \code{"full"}, or \code{"both"})
#'
#' @return a txt file
#' @export
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom rlang .data
#'


make_hr_part_H2 <- function(df, output = 'part') {

  #set up the grid of options
  combos_H2 <- expand.grid(Unitid = ipeds_unitid,
                           OccCategory5 = c(2:14),
                           REG = c(1:18),
                           Count = 0)

  #produce the uploadable format
  partH2 <- df %>%
    dplyr::filter(.data$NewHire == 1,
                  .data$FtPt == 'F',
                  .data$OccCategory5 %in% c(2:14)) %>%
    dplyr::select(.data$Unitid,
                  .data$OccCategory5,
                  .data$REG,
                  .data$Count) %>%
    #add extra combinations
    dplyr::bind_rows(combos_H2) %>%
    #aggregate the full data
    dplyr::group_by(.data$Unitid,
                    .data$OccCategory5,
                    .data$REG) %>%
    dplyr::summarize(Count = sum(.data$Count)) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$OccCategory5,
                   .data$REG) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
              SURVSECT = "SURVSECT=HR1",
              PART = "PART=H2",
              OCCCATEGORY5 = paste0("OCCCATEGORY5=", .data$OccCategory5),
              RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", .data$REG),
              COUNT = paste0("COUNT=", .data$Count))


  #create the txt file
  write_report(df = partH2,
               component = 'HumanResources',
               path = path,
               part = "PartH2",
               output = output,
               append = FALSE)
}
