
#' Produce IPEDS HR Part H1
#'
#' @description  Part H1 --- Full-time new hire instructional staff by tenure status and race/ethnicity/gender
#'
#' @param df a dataframe
#' @param output a string (\code{"part"}, \code{"full"}, or \code{"both"})
#'
#' @return a txt file
#' @export
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#'




make_hr_part_H1 <- function(df, output = 'part') {

  #set up the grid of options
  combos_H1 <- expand.grid(Unitid = ipeds_unitid,
                           Tenure = c(1:7),
                           REG = c(1:18),
                           Count = 0)

  #produce the uploadable format
  partH1 <- df %>%
    dplyr::filter(.data$Instructional == 1,
                  .data$NewHire == 1,
                  .data$FtPt == 'F') %>%
    dplyr::select(.data$Unitid,
                  .data$Tenure,
                  .data$REG,
                  .data$Count) %>%
    #add extra combinations
    dplyr::bind_rows(combos_H1) %>%
    #aggregate the full data
    dplyr::group_by(.data$Unitid,
                    .data$Tenure,
                    .data$REG) %>%
    dplyr::summarize(Count = sum(.data$Count)) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$Tenure,
                   .data$REG) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
              SURVSECT = "SURVSECT=HR1",
              PART = "PART=H1",
              TENURE = paste0("TENURE=", .data$Tenure),
              RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", .data$REG),
              COUNT = paste0("COUNT=", .data$Count))

  #create the txt file
  write_report(df = partH1,
               component = 'HumanResources',
               path = path,
               part = "PartH1",
               output = output,
               append = FALSE)
}
