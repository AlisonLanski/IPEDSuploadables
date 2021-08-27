#' Make Fall Enrollment Part C
#'
#' @description State of origin for first time students
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by summarise ungroup bind_rows arrange transmute n mutate
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'


make_ef1_part_C <- function(df, output = "part", format = "both") {

  partC_all <- df %>%
    dplyr::select(.data$Unitid,
                  .data$IsFirstTime,
                  .data$IsDegreeCertSeeking,
                  .data$StudentLevel,
                  .data$State) %>%
    dplyr::filter(.data$IsFirstTime == 1,
                  .data$IsDegreeCertSeeking == 1,
                  .data$StudentLevel == "Undergraduate") %>%
    dplyr::mutate(Line = .data$State,
                  HS = 1) %>%
    dplyr::group_by(.data$Unitid, .data$Line, .data$HS) %>%
    dplyr::summarise(Count = n()) %>%
    dplyr::ungroup()


  #have to do this separate because we DO want to double-count the recent grads in both parts
  partC_recent <-  df %>%
    dplyr::select(.data$Unitid,
                  .data$IsFirstTime,
                  .data$IsDegreeCertSeeking,
                  .data$StudentLevel,
                  .data$State,
                  .data$IsRecentGrad) %>%
    #recode_state() %>%
    dplyr::filter(.data$IsFirstTime == 1,
                  .data$IsDegreeCertSeeking == 1,
                  .data$StudentLevel == "Undergraduate",
                  .data$IsRecentGrad == 1) %>%
    dplyr::mutate(Line = .data$State,
                  HS = 2) %>%
    dplyr::group_by(.data$Unitid, .data$Line, .data$HS) %>%
    dplyr::summarise(Count = n()) %>%
    dplyr::ungroup()

  #put them together
  partC <- rbind(partC_all, partC_recent) %>%
    #remove the unknown-unknowns because the form will calc that for us
    dplyr::filter(.data$Line != 99) %>%
    #sort for easy viewing
    dplyr::arrange(.data$Line, .data$HS) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=EF1",
                     PART = "PART=C",
                     LINE = paste0("LINE=", .data$Line),
                     HS = paste0("HS=", .data$HS),
                     COUNT = paste0("COUNT=", .data$Count)
    )

  #create the txt file
  write_report(df = partC,
               component = "FallEnrollment",
               part = "PartC",
               output = output,
               format = format)
}
