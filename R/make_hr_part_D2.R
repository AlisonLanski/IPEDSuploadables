
#' Produce IPEDS HR Part D2
#'
#' @description  Part D2 --- Graduate assistants by occupational category and race/ethnicity/gender
#'
#' @param df a dataframe
#' @param output a string (\code{"part"}, \code{"full"}, or \code{"both"})
#'
#' @return a txt file
#' @export
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom rlang .data
#'



make_hr_part_D2 <- function(df, output = 'part') {

  #set up the grid of options
  combos_D2 <- expand.grid(Unitid = get_ipeds_unitid(df),
                           OccCategory4 = c(1:3),
                           REG = c(1:18),
                           Count = 0)

  #produce the uploadable format
  partD2 <- df %>%
    dplyr::filter(.data$CurrentEmployee == 1,
                  .data$OccCategory4 %in% c(1:3)) %>%
    dplyr::select(.data$Unitid,
                  .data$OccCategory4,
                  .data$REG,
                  .data$Count) %>%
    #add extra combinations
    dplyr::bind_rows(combos_D2) %>%
    #aggregate the full data
    dplyr::group_by(.data$Unitid,
                    .data$OccCategory4,
                    .data$REG) %>%
    dplyr::summarize(Count = sum(.data$Count)) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$OccCategory4,
                   .data$REG) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
              SURVSECT = "SURVSECT=HR1",
              PART = "PART=D2",
              OCCCATEGORY4 = paste0("OCCCATEGORY4=", .data$OccCategory4),
              RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", .data$REG),
              COUNT = paste0("COUNT=", .data$Count))
  #create the txt file
  write_report(df = partD2,
               component = 'HumanResources',
               part = "PartD2",
               output = output)
}
