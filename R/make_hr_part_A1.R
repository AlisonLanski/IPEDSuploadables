##
#' Produce IPEDS HR Part A1
#'
#' @description  Part A1 --- Count of FT Instructional staff by tenure status, academic rank, and race/ethnicity/gender
#'
#' @param df a dataframe
#' @param output a string (\code{"part"}, \code{"full"}, or \code{"both"})
#'
#' @return a txt file
#' @export
#' @importFrom dplyr bind_rows filter select group_by ungroup arrange transmute
#' @importFrom rlang .data
#'
#'
make_hr_part_A1 <- function(df, output = "part") {

  ipeds_unitid <- get_ipeds_unitid(df)

  #set up the grid of options
  combos_A1 <- expand.grid(Unitid = ipeds_unitid,
                           Tenure = c(1:7),
                           Rank = c(1:6),
                           REG = c(1:18),
                           Count = 0) %>%
               dplyr::bind_rows(expand.grid(Unitid = ipeds_unitid,
                                     Tenure = 6,
                                     Rank = 7,
                                     REG = 1:18,
                                     Count = 0)
                         )

  #produce the uploadable format
  partA1 <- df %>%
            dplyr::filter(.data$CurrentEmployee == 1,
                          .data$Instructional == 1,
                          .data$FtPt == 'F') %>%
            dplyr::select(.data$Unitid,
                          .data$Tenure,
                          .data$Rank,
                          .data$REG,
                          .data$Count) %>%
            #add extra combinations
            dplyr::bind_rows(combos_A1) %>%
            #aggregate the full data
            dplyr::group_by(.data$Unitid,
                            .data$Tenure,
                            .data$Rank,
                            .data$REG) %>%
            dplyr::summarize(Count = sum(.data$Count)) %>%
            dplyr::ungroup() %>%
            #sort for easy viewing
            dplyr::arrange(.data$Tenure, .data$Rank, .data$REG) %>%
            #format for upload
            dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                      SURVSECT = "SURVSECT=HR1",
                      PART = "PART=A1",
                      TENURE = paste0("TENURE=", .data$Tenure),
                      RANK = paste0("RANK=", .data$Rank),
                      RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", .data$REG),
                      COUNT = paste0("COUNT=", .data$Count))

      write_report(df = partA1,
                   component = 'HumanResources',
                   part = "PartA1",
                   output = output)



}
