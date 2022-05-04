#' Make Human Resources Part D1
#'
#' @description  Part D1 --- Part-time staff by occupational category
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

make_hr_part_D1 <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #set up the grid of options
  combos_D1 <- expand.grid(UNITID = get_ipeds_unitid(df),
                           OCCCATEGORY1 = c(1:18),
                           REG = c(1:18),
                           COUNT = 0)

  #produce the uploadable format
  partD1 <- df %>%
            dplyr::filter(.data$CURRENTEMPLOYEE == 1,
                          .data$FTPT == "P",
                          .data$OCCCATEGORY1 %in% c(1:18)) %>%
            dplyr::select(.data$UNITID,
                          .data$OCCCATEGORY1,
                          .data$REG,
                          .data$COUNT) %>%
            #add extra combinations
            dplyr::bind_rows(combos_D1) %>%
            #aggregate the full data
            dplyr::group_by(.data$UNITID,
                            .data$OCCCATEGORY1,
                            .data$REG) %>%
            dplyr::summarize(COUNT = sum(.data$COUNT)) %>%
            dplyr::ungroup() %>%
            #sort for easy viewing
            dplyr::arrange(.data$OCCCATEGORY1,
                           .data$REG) %>%
            #format for upload
            dplyr::transmute(UNITID = .data$UNITID,
                             SURVSECT = "HR1",
                             PART = "D1",
                             OCCCATEGORY1 = .data$OCCCATEGORY1,
                             RACEETHNICITYGENDER = .data$REG,
                             COUNT = .data$COUNT
                             )

}
