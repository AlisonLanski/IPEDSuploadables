##
#' Produce IPEDS HR Part B1
#'
#' @description  HR Part B1 --- Count of FT Non-instructional staff by occupational category
#'
#' @param df a dataframe
#' @param output a string (\code{"part"}, \code{"full"}, or \code{"both"})
#'
#' @return a txt file
#' @export
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom rlang .data
#'
make_hr_part_B1 <- function(df, output = 'part') {

  #set up the grid of options
  combos_B1 <- expand.grid(Unitid = get_ipeds_unitid(df),
                           OccCategory1 = c(1:17),
                           REG = c(1:18),
                           Count = 0)

  #produce the uploadable format
  partB1 <- df %>%
            dplyr::filter(.data$CurrentEmployee == 1,
                          .data$Instructional == 0, #non-instructional
                          .data$FtPt == 'F') %>%
            dplyr::select(.data$Unitid,
                          .data$OccCategory1,
                          .data$REG,
                          .data$Count) %>%
            #add extra combinations
            dplyr::bind_rows(combos_B1) %>%
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
                      PART = "PART=B1",
                      OCCCATEGORY1 = paste0("OCCCATEGORY1=", .data$OccCategory1),
                      RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", .data$REG),
                      COUNT = paste0("COUNT=", .data$Count))

  write_report(df = partB1,
               component = 'HumanResources',
               part = "PartB1",
               output = output)
}
