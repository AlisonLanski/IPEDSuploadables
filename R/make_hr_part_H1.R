#' Make Human Resources Part H1
#'
#' @description  Part H1 --- Full-time new hire instructional staff by tenure status and race/ethnicity/gender
#'
#' @param df a dataframe
#'
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom stringr str_to_upper
#'
#' @return a txt file
#' @export
#'

make_hr_part_H1 <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #set up the grid of options
  combos_H1 <- expand.grid(UNITID = get_ipeds_unitid(df),
                           TENURE = c(1:7),
                           REG = c(1:18),
                           COUNT = 0)

  #produce the uploadable format
  partH1 <- df %>%
            dplyr::filter(.data$INSTRUCTIONAL == 1,
                          .data$NEWHIRE == 1,
                          .data$FTPT == "F") %>%
            dplyr::select("UNITID",
                          "TENURE",
                          "REG",
                          "COUNT") %>%
            #add extra combinations
            dplyr::bind_rows(combos_H1) %>%
            #aggregate the full data
            dplyr::group_by(.data$UNITID,
                            .data$TENURE,
                            .data$REG) %>%
            dplyr::summarize(COUNT = sum(.data$COUNT)) %>%
            dplyr::ungroup() %>%
            #sort for easy viewing
            dplyr::arrange(.data$TENURE,
                           .data$REG) %>%
            #format for upload
            dplyr::transmute(UNITID = .data$UNITID,
                             SURVSECT = "HR1",
                             PART = "H1",
                             TENURE = .data$TENURE,
                             RACEETHNICITYGENDER = .data$REG,
                             COUNT = .data$COUNT
                             )
}
