#' Produce IPEDS HR Part H2
#'
#' @description  Part H2 --- New hires by occupational category, Race/Ethnicity/Gender
#'
#' @param df a dataframe
#' @param output a string (\code{"part"}, \code{"full"}, or \code{"both"})
#'
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom rlang .data
#' @importFrom stringr str_to_upper
#'
#' @return a txt file
#' @export
#'

make_hr_part_H2 <- function(df, output = "part", format = "both") {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #set up the grid of options
  combos_H2 <- expand.grid(UNITID = get_ipeds_unitid(df),
                           OCCCATEGORY5 = c(2:14),
                           REG = c(1:18),
                           COUNT = 0)

  #produce the uploadable format
  partH2 <- df %>%
            dplyr::filter(.data$NEWHIRE == 1,
                          .data$FTPT == "F",
                          .data$OCCCATEGORY5 %in% c(2:14)) %>%
            dplyr::select(.data$UNITID,
                          .data$OCCCATEGORY5,
                          .data$REG,
                          .data$COUNT) %>%
            #add extra combinations
            dplyr::bind_rows(combos_H2) %>%
            #aggregate the full data
            dplyr::group_by(.data$UNITID,
                            .data$OCCCATEGORY5,
                            .data$REG) %>%
            dplyr::summarize(COUNT = sum(.data$COUNT)) %>%
            dplyr::ungroup() %>%
            #sort for easy viewing
            dplyr::arrange(.data$OCCCATEGORY5,
                           .data$REG) %>%
            #format for upload
            dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                             SURVSECT = "SURVSECT=HR1",
                             PART = "PART=H2",
                             OCCCATEGORY5 = paste0("OCCCATEGORY5=", .data$OCCCATEGORY5),
                             RACEETHNICITYGENDER = paste0("RACEETHNICITYGENDER=", .data$REG),
                             COUNT = paste0("COUNT=", .data$COUNT))

  #create the txt file
  write_report(df = partH2,
               component = "HumanResources",
               part = "PartH2",
               output = output,
               format = format)
}
