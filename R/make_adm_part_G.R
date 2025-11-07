#' Make Admissions Part G (Transfer sex unknown)
#'
#' @description Counts the number of unknown sex for Transfer students
#'
#' @param df A dataframe of applicant information
#'
#' @return Admissions Part G data with the required IPEDS structure
#'
#' @export
#'

make_adm_part_G <- function(df) {


  ### Code is the SAME as part D -- only change is the part letters and
  ### a filter for ISTRANSFER instead of ISFIRSTTIME

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partG_AppAdm <- df %>%
    dplyr::filter(.data$ISTRANSFER  == 1,
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


  partG_Enrl <- df %>%
    dplyr::filter(.data$ISTRANSFER  == 1,
                  .data$GENDERDETAIL == 3) %>%
    dplyr::select("UNITID",
                  "ISENROLLED",
                  "ISFULLTIME"
    ) %>%
    dplyr::group_by(.data$UNITID, .data$ISFULLTIME) %>%
    dplyr::summarize(COUNT = sum(.data$ISENROLLED, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Line = ifelse(.data$ISFULLTIME == 1, 3, 4))


  partG <- dplyr::bind_rows(partG_AppAdm, partG_Enrl) %>%
    dplyr::transmute(UNITID = .data$UNITID,
                     SURVSECT = 'ADM',
                     PART = "G",
                     LINE = .data$Line,
                     ADMSEX = .data$COUNT) %>%
    dplyr::arrange(.data$LINE)

  if (all(is.na(df)) == TRUE) {
    return()
  } else {
    return(partG)
  }
}
