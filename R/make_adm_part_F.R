#' Make Admissions Part F (Transfer student demographics)
#'
#' @description Aggregates Transfer student applicants by sex, race/ethnicity.
#'
#' @param df A dataframe of applicant information
#'
#' @return Admissions Part F data with the required IPEDS structure
#'
#' @export
#'


make_adm_part_F <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  ## transfer students for part F: all applicants/admits by RE & Sex
  partF_AppAdm <- df %>%
    dplyr::filter(.data$ISTRANSFER == 1) %>%
    dplyr::select("UNITID",
                  "SEX",
                  "RACEETHNICITY",
                  "ISAPPLICANT",
                  "ISADMITTED"
    ) %>%
    dplyr::group_by(.data$UNITID,
                    .data$RACEETHNICITY,
                    .data$SEX
    ) %>%
    dplyr::summarize(ApplyCount = dplyr::n(),
                     AdmitCount = sum(.data$ISADMITTED, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = c("ApplyCount", "AdmitCount"), names_to = 'Metric', values_to = 'COUNT') %>%
    mutate(Line = ifelse(.data$Metric == 'ApplyCount', 1, 2))

  # transfer student for F: all enrolled by RE/Sex & Time Status
  partF_Enrl <- df %>%
    dplyr::filter(.data$ISTRANSFER == 1) %>%
    dplyr::select("UNITID",
                  "SEX",
                  "RACEETHNICITY",
                  "ISFULLTIME",
                  "ISENROLLED"
    ) %>%
    dplyr::group_by(.data$UNITID,
                    .data$RACEETHNICITY,
                    .data$SEX,
                    .data$ISFULLTIME
    ) %>%
    dplyr::summarize(EnrollCount = sum(.data$ISENROLLED, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Line = dplyr::case_when(ISFULLTIME == 1 ~ 3,
                                          ISFULLTIME == 0 ~ 4,
                                          TRUE ~ 99)) %>%
    dplyr::select(-"ISFULLTIME") %>%
    tidyr::pivot_longer(cols = c("EnrollCount"), names_to = 'Metric', values_to = 'COUNT')

  # Combine all lines
  partF <- dplyr::bind_rows(partF_AppAdm, partF_Enrl) %>%
    # not necessary to remove 0s, but cleans it up
    dplyr::filter(.data$COUNT > 0) %>%
    # final form
    dplyr::transmute(UNITID = .data$UNITID,
                     SURVSECT = "ADM",
                     PART = "F",
                     LINE = .data$Line,
                     RACE = .data$RACEETHNICITY,
                     SEX = .data$SEX,
                     COUNT = .data$COUNT
    ) %>%
    dplyr::arrange(.data$LINE, .data$RACE, .data$SEX)

  return(partF)

}
