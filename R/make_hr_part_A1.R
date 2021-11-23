#' Produce IPEDS HR Part A1
#'
#' @description  Part A1 --- COUNT of FT INSTRUCTIONAL staff by tenure status, academic rank, and race/ethnicity/gender
#'
#' @param df a dataframe
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom dplyr bind_rows filter select group_by ungroup arrange transmute
#' @importFrom rlang .data
#' @importFrom stringr str_to_upper
#'
#' @return a txt file
#' @export
#'

make_hr_part_A1 <- function(df, output = "part", format = "both") {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  ipeds_unitid <- get_ipeds_unitid(df)

  #set up the grid of options
  combos_A1 <- expand.grid(UNITID = ipeds_unitid,
                           TENURE = c(1:7),
                           RANK = c(1:6),
                           REG = c(1:18),
                           COUNT = 0) %>%
               dplyr::bind_rows(expand.grid(UNITID = ipeds_unitid,
                                            TENURE = 6,
                                            RANK = 7,
                                            REG = 1:18,
                                            COUNT = 0))

  #produce the uploadable format
  partA1 <- df %>%
            dplyr::filter(.data$CURRENTEMPLOYEE == 1,
                          .data$INSTRUCTIONAL == 1,
                          .data$FTPT == "F") %>%
            dplyr::select(.data$UNITID,
                          .data$TENURE,
                          .data$RANK,
                          .data$REG,
                          .data$COUNT) %>%
            #add extra combinations
            dplyr::bind_rows(combos_A1) %>%
            #aggregate the full data
            dplyr::group_by(.data$UNITID,
                            .data$TENURE,
                            .data$RANK,
                            .data$REG) %>%
            dplyr::summarize(COUNT = sum(.data$COUNT)) %>%
            dplyr::ungroup() %>%
            #sort for easy viewing
            dplyr::arrange(.data$TENURE,
                           .data$RANK,
                           .data$REG) %>%
            #format for upload
            dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                             SURVSECT = "SURVSECT=HR1",
                             PART = "PART=A1",
                             TENURE = paste0("TENURE=", .data$TENURE),
                             RANK = paste0("RANK=", .data$RANK),
                             RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", .data$REG),
                             COUNT = paste0("COUNT=", .data$COUNT))

  write_report(df = partA1,
               component = "HumanResources",
               part = "PartA1",
               output = output,
               format = format)

}
