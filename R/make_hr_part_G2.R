
#' Produce IPEDS HR Part G2
#'
#' @description  Part G2 --- Salaries of non-instructional staff
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



make_hr_part_G2 <- function(df, output = 'part') {

  #set up the grid of options
  combos_G2 <- expand.grid(Unitid = get_ipeds_unitid(df),
                           OccCategory2 = c(1:13),
                           Salary = 0,
                           EmpId = 'dummy_id',
                           Count = 0)

  #produce the uploadable format
  partG2 <- df %>%
    dplyr::filter(.data$CurrentEmployee == 1,
                  .data$Instructional == 0, #non-instructional
                  .data$FtPt == "F") %>%  #not listed in uploadable instructions -- verify?
    dplyr::select(.data$Unitid,
                  .data$OccCategory2,
                  .data$Salary,
                  .data$EmpId,
                  .data$Count) %>%
    #add extra combinations
    dplyr::bind_rows(combos_G2) %>%
    #aggregate the full data
    dplyr::group_by(.data$Unitid,
                    .data$OccCategory2) %>%
    dplyr::summarize(Salary = sum(.data$Salary)) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$OccCategory2) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
              SURVSECT = "SURVSECT=HR1",
              PART = "PART=G2",
              OCCCATEGORY2 = paste0("OCCCATEGORY2=", .data$OccCategory2),
              SOUTLAYS = paste0("SOUTLAYS=", .data$Salary))


  #create the txt file
  write_report(df = partG2,
               component = 'HumanResources',
               part = "PartG2",
               output = output)
}
