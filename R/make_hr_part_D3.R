#' Produce IPEDS HR Part D3
#'
#' @description  Part D3 --- Part-time staff by tenure, medical school, and occupational category
#'
#' @param df a dataframe
#'
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom rlang .data
#' @importFrom stringr str_to_upper
#'
#' @return a txt file
#' @export
#'

make_hr_part_D3 <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #set up the grid of options
  combos_D3 <- expand.grid(UNITID = get_ipeds_unitid(df),
                           TENURE = c(1:7),
                           ISMEDICAL = c(0:1),
                           OCCCATEGORY3 = c(1:15),
                           COUNT = 0)

  #produce the uploadable format
  partD3 <- df %>%
            dplyr::filter(.data$CURRENTEMPLOYEE == 1,
                          .data$FTPT == "P",
                          .data$OCCCATEGORY3 %in% c(1:15)) %>%
            dplyr::select(.data$UNITID,
                          .data$TENURE,
                          .data$ISMEDICAL,
                          .data$OCCCATEGORY3,
                          .data$COUNT) %>%
            #add extra combinations
            dplyr::bind_rows(combos_D3) %>%
            #aggregate the full data
            dplyr::group_by(.data$UNITID,
                            .data$TENURE,
                            .data$ISMEDICAL,
                            .data$OCCCATEGORY3) %>%
            dplyr::summarize(COUNT = sum(.data$COUNT)) %>%
            dplyr::ungroup() %>%
            #sort for easy viewing
            dplyr::arrange(.data$TENURE,
                           .data$ISMEDICAL,
                           .data$OCCCATEGORY3) %>%
            #format for upload
            dplyr::transmute(UNITID = .data$UNITID,
                             SURVSECT = "HR1",
                             PART = "D3",
                             TENURE = .data$TENURE,
                             ISMEDICAL = .data$ISMEDICAL,
                             OCCCATEGORY3 = .data$OCCCATEGORY3,
                             COUNT = .data$COUNT
                             )

}
