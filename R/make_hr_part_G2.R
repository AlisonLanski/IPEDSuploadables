#' Produce IPEDS HR Part G2
#'
#' @description  Part G2 --- Salaries of non-instructional staff
#'
#' @param df a dataframe
#' @param output a string (\code{"part"}, \code{"full"}, or \code{"both"})
#'
#' @importFrom dplyr bind_rows filter select bind_rows group_by summarize ungroup arrange transmute
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom stringr str_to_upper
#'
#' @return a txt file
#' @export
#'

make_hr_part_G2 <- function(df, output = "part", format = "both") {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #set up the grid of options
  combos_G2 <- expand.grid(UNITID = get_ipeds_unitid(df),
                           OCCCATEGORY2 = c(1:13),
                           SALARY = 0,
                           EMPID = "dummy_id",
                           COUNT = 0)

  #produce the uploadable format
  partG2 <- df %>%
            dplyr::filter(.data$CURRENTEMPLOYEE == 1,
                          .data$INSTRUCTIONAL == 0, #non-instructional
                          .data$FTPT == "F") %>%  #not listed in uploadable instructions -- verify?
            dplyr::select(.data$UNITID,
                          .data$OCCCATEGORY2,
                          .data$SALARY,
                          .data$EMPID,
                          .data$COUNT) %>%
            #add extra combinations
            dplyr::bind_rows(combos_G2) %>%
            #aggregate the full data
            dplyr::group_by(.data$UNITID,
                            .data$OCCCATEGORY2) %>%
            dplyr::summarize(SALARY = sum(.data$SALARY)) %>%
            dplyr::ungroup() %>%
            #sort for easy viewing
            dplyr::arrange(.data$OCCCATEGORY2) %>%
            #format for upload
            dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                             SURVSECT = "SURVSECT=HR1",
                             PART = "PART=G2",
                             OCCCATEGORY2 = paste0("OCCCATEGORY2=", .data$OCCCATEGORY2),
                             SOUTLAYS = paste0("SOUTLAYS=", .data$SALARY))

  #create the txt file
  write_report(df = partG2,
               component = "HumanResources",
               part = "PartG2",
               output = output,
               format = format)
}
