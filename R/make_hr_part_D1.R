##
#' Produce IPEDS HR Part D1
#'
#' @description  Part D1 --- Part-time staff by occupational category
#'
#' @param df a dataframe
#' @param output a string (\code{"part"}, \code{"full"}, or \code{"both"})
#'
#' @return a txt file
#' @export
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom rlang .data
#'

make_hr_part_D1 <- function(df, output = 'part') {

  #set up the grid of options
  combos_D1 <- expand.grid(Unitid = ipeds_unitid,
                           OccCategory1 = c(1:18),
                           REG = c(1:18),
                           Count = 0)

  #produce the uploadable format
  partD1 <- df %>%
    dplyr::filter(.data$CurrentEmployee == 1,
                  .data$FtPt == 'P',
                  .data$OccCategory1 %in% c(1:18)) %>%
    dplyr::select(.data$Unitid,
                  .data$OccCategory1,
                  .data$REG,
                  .data$Count) %>%
    #add extra combinations
    dplyr::bind_rows(combos_D1) %>%
    #aggregate the full data
    dplyr::group_by(.data$Unitid,
                    .data$OccCategory1,
                    .data$REG) %>%
    dplyr::summarize(Count = sum(.data$Count)) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$OccCategory1,
                   .data$REG) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
              SURVSECT = "SURVSECT=HR1",
              PART = "PART=D1",
              OCCCATEGORY1 = paste0("OCCCATEGORY1=", .data$OccCategory1),
              RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", .data$REG),
              COUNT = paste0("COUNT=", .data$Count))

  #create the txt file
  write_report(df = partD1,
               component = 'HumanResources',
               path = path,
               part = "PartD1",
               output = output,
               append = FALSE)
}
