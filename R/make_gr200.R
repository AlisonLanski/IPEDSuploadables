#' Make Graduation Rates 200
#'
#' @param df A dataframe of student/degree information
#' @param extracips A dataframe of cips offered by the institution but not in \code{'df'}
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by summarize ungroup bind_rows arrange transmute n
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'


make_gr200 <- function(df, output = "part", format = "both") {

  #produce the uploadable format
  exclusions <- df %>%
    dplyr::filter(.data$Is_Exclusion == 1) %>%
    dplyr::group_by(.data$Is_Exclusion) %>%
    dplyr::summarize(Exclusions = dplyr::n()) %>%
    ungroup()

  comp <- df %>%
    dplyr::filter(.data$Is_Comp == 1) %>%
    dplyr::group_by(.data$Is_Comp) %>%
    dplyr::summarize(Comp = dplyr::n()) %>%
    ungroup()

  se <- df %>%
    dplyr::filter(.data$Is_Still_Enrolled == 1) %>%
    dplyr::group_by(.data$Is_Still_Enrolled) %>%
    dplyr::summarize(StillEnrolled = dplyr::n()) %>%
    ungroup()

  gr200 <- df %>%
    dplyr::select(.data$Unitid) %>%
    dplyr::distinct() %>%
    dplyr::bind_cols(exclusions, comp, se) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=GR21",
                     PART = "PART=A",
                     ADEXCL = paste0("ADEXCL=", .data$Exclusions),
                     `COMPY7-8` = paste0("COMPY7-8=", .data$Comp),
                     STILLENROLLED = paste0("STILLENROLLED=", .data$StillEnrolled),
                     )

  #create the txt file
  write_report(df = gr200,
               component = "GR200",
               part = "AllData",
               output = output,
               format = format)
}
