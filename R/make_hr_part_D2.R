#' Make Human Resources Part D2
#'
#' @description  Part D2 --- Graduate assistants by occupational category and race/ethnicity/gender
#'
#' @param df a dataframe
#'
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom rlang .data
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_hr_part_D2 <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #set up the grid of options
  combos_D2 <- expand.grid(UNITID = get_ipeds_unitid(df),
                           OCCCATEGORY4 = c(1:3),
                           REG = c(1:18),
                           COUNT = 0)

  #produce the uploadable format
  partD2 <- df %>%
            dplyr::filter(.data$CURRENTEMPLOYEE == 1,
                          .data$OCCCATEGORY4 %in% c(1:3)) %>%
            dplyr::select("UNITID",
                          "OCCCATEGORY4",
                          "REG",
                          "COUNT") %>%
            #add extra combinations
            dplyr::bind_rows(combos_D2) %>%
            #aggregate the full data
            dplyr::group_by(.data$UNITID,
                            .data$OCCCATEGORY4,
                            .data$REG) %>%
            dplyr::summarize(COUNT = sum(.data$COUNT)) %>%
            dplyr::ungroup() %>%
            #sort for easy viewing
            dplyr::arrange(.data$OCCCATEGORY4,
                           .data$REG) %>%
            #format for upload
            dplyr::transmute(UNITID = .data$UNITID,
                             SURVSECT = "HR1",
                             PART = "D2",
                             OCCCATEGORY4 = .data$OCCCATEGORY4,
                             RACEETHNICITYGENDER = .data$REG,
                             COUNT = .data$COUNT
                             )

}
