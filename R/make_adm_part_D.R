#' Make Admissions Part D (First-time sex unknown)
#'
#' @description Counts the number of unknown sex for First-time students
#'
#' @param df A dataframe of applicant information
#'
#' @return Admissions Part D data with the required IPEDS structure
#'
#' @export
#'

make_adm_part_D <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  ### Code is the SAME as part G -- only change is the part letters and
  ### a filter for ISFIRSTTIME instead of ISTRANSFER

  partD_AppAdm <- df %>%
    dplyr::filter(.data$ISFIRSTTIME  == 1,
                  .data$GENDERDETAIL == 3) %>%
    dplyr::select("UNITID",
                  "ISAPPLICANT",
                  "ISADMITTED"
    ) %>%
    dplyr::group_by(.data$UNITID) %>%
    dplyr::summarize(ApplyCount = sum(.data$ISAPPLICANT, na.rm = TRUE),
                     AdmitCount = sum(.data$ISADMITTED, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = c("ApplyCount", "AdmitCount"), names_to = 'Metric', values_to = 'COUNT') %>%
    dplyr::mutate(Line = ifelse(.data$Metric == 'ApplyCount', 1, 2))


  partD_Enrl <- df %>%
    dplyr::filter(.data$ISFIRSTTIME  == 1,
                  .data$GENDERDETAIL == 3) %>%
    dplyr::select("UNITID",
                  "ISENROLLED",
                  "ISFULLTIME"
    ) %>%
    dplyr::group_by(.data$UNITID, .data$ISFULLTIME) %>%
    dplyr::summarize(COUNT = sum(.data$ISENROLLED, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Line = ifelse(.data$ISFULLTIME == 1, 3, 4))


  partD <- dplyr::bind_rows(partD_AppAdm, partD_Enrl) %>%
    dplyr::transmute(UNITID = .data$UNITID,
                     SURVSECT = 'ADM',
                     PART = "D",
                     LINE = .data$Line,
                     ADMSEX = .data$COUNT) %>%
    dplyr::arrange(.data$LINE)

  if (all(is.na(df)) == TRUE) {
    return()
  } else {
  return(partD)
  }
}
