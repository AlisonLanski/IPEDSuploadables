#' Make Human Resources Part D4
#'
#' @description  Part D4 --- Part-time Non-instructional staff by medical school, and occupational category
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

make_hr_part_D4 <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #set up the grid of options
  combos_D4 <- expand.grid(UNITID = get_ipeds_unitid(df),
                           ISMEDICAL = c(0:1),
                           OCCCATEGORY3 = c(16:20, 22:24),
                           COUNT = 0)

  #produce the uploadable format
  partD4 <- df %>%
            dplyr::filter(.data$CURRENTEMPLOYEE == 1,
                          .data$FTPT == "P",
                          .data$OCCCATEGORY3 %in% c(16:20, 22:24)) %>%
            dplyr::select(.data$UNITID,
                          .data$ISMEDICAL,
                          .data$OCCCATEGORY3,
                          .data$COUNT) %>%
            #add extra combinations
            dplyr::bind_rows(combos_D4) %>%
            #aggregate the full data
            dplyr::group_by(.data$UNITID,
                            .data$ISMEDICAL,
                            .data$OCCCATEGORY3) %>%
            dplyr::summarize(COUNT = sum(.data$COUNT)) %>%
            dplyr::ungroup() %>%
            #sort for easy viewing
            dplyr::arrange(.data$ISMEDICAL,
                           .data$OCCCATEGORY3) %>%
            #format for upload
            dplyr::transmute(UNITID = .data$UNITID,
                             SURVSECT = "HR1",
                             PART = "D4",
                             ISMEDICAL = .data$ISMEDICAL,
                             OCCCATEGORY3 = .data$OCCCATEGORY3,
                             COUNT = .data$COUNT
                             )

}
