##
#' Produce IPEDS HR Part B2
#'
#' @description  Part B2 --- Full-time non-instructional staff by tenure, medical school, and occupational category
#'
#' @param df a dataframe
#' @param output a string (\code{"part"}, \code{"full"}, or \code{"both"})
#'
#' @return a txt file
#' @export
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom rlang .data
#'

make_hr_part_B2 <- function(df, output = 'part') {

    #set up the grid of options
    combos_B2 <- expand.grid(Unitid = ipeds_unitid,
                             Tenure = c(1:7),
                             IsMedical = c(0:1),
                             OccCategory1 = c(2:12),
                             Count = 0)

    #produce the uploadable format
    partB2 <- df %>%
              dplyr::filter(.data$CurrentEmployee == 1,
                            .data$Instructional == 0,
                            .data$FtPt == 'F',
                            .data$OccCategory1 %in% c(2:12)) %>%
              dplyr::select(.data$Unitid,
                            .data$Tenure,
                            .data$IsMedical,
                            .data$OccCategory1,
                            .data$Count) %>%
              #add extra combinations
              dplyr::bind_rows(combos_B2) %>%
              #aggregate the full data
              dplyr::group_by(.data$Unitid,
                              .data$Tenure,
                              .data$IsMedical,
                              .data$OccCategory1) %>%
              dplyr::summarize(Count = sum(.data$Count)) %>%
              dplyr::ungroup() %>%
              #sort for easy viewing
              dplyr::arrange(.data$Tenure,
                             .data$IsMedical,
                             .data$OccCategory1) %>%
              #format for upload
              dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                        SURVSECT = "SURVSECT=HR1",
                        PART = "PART=B2",
                        TENURE = paste0("TENURE=", .data$Tenure),
                        ISMEDICAL = paste0("ISMEDICAL=", .data$IsMedical),
                        OCCCATEGORY1 = paste0("OCCCATEGORY1=", .data$OccCategory1),
                        COUNT = paste0("COUNT=", .data$Count))

  #create the txt file
  write_report(df = partB2,
               component = 'HumanResources',
               path = path,
               part = "PartB2",
               output = output,
               append = FALSE)
}
