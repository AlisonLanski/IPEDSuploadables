
#' Produce IPEDS HR Part D3
#'
#' @description  Part D3 --- Part-time staff by tenure, medical school, and occupational category
#'
#' @param df a dataframe
#' @param output a string (\code{"part"}, \code{"full"}, or \code{"both"})
#'
#' @return a txt file
#' @export
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom rlang .data
#'



make_hr_part_D3 <- function(df,output = 'part') {

  #set up the grid of options
  combos_D3 <- expand.grid(Unitid = ipeds_unitid,
                           Tenure = c(1:7),
                           IsMedical = c(0:1),
                           OccCategory3 = c(1:15),
                           Count = 0)

  #produce the uploadable format
  partD3 <- df %>%
    dplyr::filter(.data$CurrentEmployee == 1,
                  .data$FtPt == "P",
                  .data$OccCategory3 %in% c(1:15)) %>%
    dplyr::select(.data$Unitid,
                  .data$Tenure,
                  .data$IsMedical,
                  .data$OccCategory3,
                  .data$Count) %>%
    #add extra combinations
    dplyr::bind_rows(combos_D3) %>%
    #aggregate the full data
    dplyr::group_by(.data$Unitid,
                    .data$Tenure,
                    .data$IsMedical,
                    .data$OccCategory3) %>%
    dplyr::summarize(Count = sum(.data$Count)) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$Tenure,
                   .data$IsMedical,
                   .data$OccCategory3) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
              SURVSECT = "SURVSECT=HR1",
              PART = "PART=D3",
              TENURE = paste0("TENURE=", .data$Tenure),
              ISMEDICAL = paste0("ISMEDICAL=", .data$IsMedical),
              OCCCATEGORY3 = paste0("OCCCATEGORY3=", .data$OccCategory3),
              COUNT = paste0("COUNT=", .data$Count))


  #create the txt file
  write_report(df = partD3,
               component = 'HumanResources',
               path = path,
               part = "PartD3",
               output = output,
               append = FALSE)
}
