#' Make Admissions Part H (Transfer SAT/ACT scores)
#'
#' @description If SAT or ACT scores were used for admission decisions it calculates the requisite percentiles for submission.
#'
#' @param This function relies on the student data frame that conforms to the documented specifications
#'        for the Admissions module of the IPEDS Uploadables package.
#'
#' @return Part H data prepared for inclusion in the final text file for uploading to the IPEDS portal.
#'
#' @export
#'

make_adm_part_H <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  # select transfer students
  dfsat <- filter(df, .data$ISTRANSFER == 1, .data$ISENROLLED == 1, .data$SATUSED == 1)

  partH_SAT <- (dfsat) %>%
    dplyr::select(UNITID,
                  STUDENTID,
                  ISENROLLED,
                  SATUSED,
                  ACTUSED,
                  SAT_EVBRW,
                  SAT_MATH,
                )%>%
    dplyr::group_by(.data$UNITID)%>%
    # Add Rounding logic
    dplyr::summarize(SATINUM = n(),
                     SATVR25 = as.integer(round(quantile(.data$SAT_EVBRW, 0.25)), digits = 0),
                     SATVR50 = as.integer(round(quantile(.data$SAT_EVBRW, 0.50)), digits = 0),
                     SATVR75 = as.integer(round(quantile(.data$SAT_EVBRW, 0.75)), digits = 0),
                     SATMT25 = as.integer(round(quantile(.data$SAT_MATH, 0.25)), digits = 0),
                     SATMT50 = as.integer(round(quantile(.data$SAT_MATH, 0.50)), digits = 0),
                     SATMT75 = as.integer(round(quantile(.data$SAT_MATH, 0.75)), digits = 0)
    )


  dfact <- filter(df, .data$ISTRANSFER == 1, .data$ISENROLLED == 1, .data$ACTUSED == 1)
  partH_ACT <- (dfact) %>%
    dplyr::select(UNITID,
                  STUDENTID,
                  ISENROLLED,
                  ACTUSED,
                  ACT_ENG,
                  ACT_COMP,
                  ACT_MATH)%>%
    dplyr::group_by(.data$UNITID)%>%
    # Add rounding logic
    dplyr::summarize(ACTINUM = n(),
                     ACTCM25 = as.integer(round(quantile(.data$ACT_COMP, 0.25)), digits = 0),
                     ACTCM50 = as.integer(round(quantile(.data$ACT_COMP, 0.50)), digits = 0),
                     ACTCM75 = as.integer(round(quantile(.data$ACT_COMP, 0.75)), digits = 0),
                     ACTMT25 = as.integer(round(quantile(.data$ACT_MATH, 0.25)), digits = 0),
                     ACTMT50 = as.integer(round(quantile(.data$ACT_MATH, 0.50)), digits = 0),
                     ACTMT75 = as.integer(round(quantile(.data$ACT_MATH, 0.75)), digits = 0),
                     ACTEN25 = as.integer(round(quantile(.data$ACT_ENG, 0.25)), digits = 0),
                     ACTEN50 = as.integer(round(quantile(.data$ACT_ENG, 0.50)), digits = 0),
                     ACTEN75 = as.integer(round(quantile(.data$ACT_ENG, 0.75)), digits = 0)
    )
  # find total transfers for denominator
  tr <- filter(df, .data$ISTRANSFER == 1, .data$ISENROLLED == 1) %>%
    dplyr::summarize(COUNT = n())
  # Now do Part E for transfers
  #format for upload
  partH_prep <- dplyr::bind_cols(partH_SAT,
                                 select(partH_ACT, -UNITID)
  ) %>%
    ### need to add logic for 5 or less to SATIPCT and ACTIPCT
    dplyr::mutate(SATIPCT = as.integer(round((.data$SATINUM/tr)*100), digits = 0)) %>%
    dplyr::mutate(ACTIPCT = as.integer(round((.data$ACTINUM/tr)*100), digits = 0)) %>%
    dplyr::mutate(SATVR25 = dplyr::case_when(
      .data$SATINUM > 5 ~ .data$SATVR25,
      .data$SATINUM <= 5 ~ -2)) %>%
    dplyr::mutate(SATVR75 = dplyr::case_when(
      .data$SATINUM > 5 ~ .data$SATVR75,
      .data$SATINUM <= 5 ~ -2)) %>%
    dplyr::mutate(SATVR50 = dplyr::case_when(
      .data$SATINUM > 5 ~ .data$SATVR50,
      .data$SATINUM <= 5 ~ -2)) %>%
    dplyr::mutate(ACTEN25 = dplyr::case_when(
      .data$ACTINUM > 5 ~ .data$ACTEN25,
      .data$ACTINUM <= 5 ~ -2)) %>%
    dplyr::mutate(ACTEN75 = dplyr::case_when(
      .data$ACTINUM > 5 ~ .data$ACTEN75,
      .data$ACTINUM <= 5 ~ -2)) %>%
    dplyr::mutate(ACTEN50 = dplyr::case_when(
      .data$ACTINUM > 5 ~ .data$ACTEN50,
      .data$ACTINUM <= 5 ~ -2))

  partH <- partH_prep %>%
    dplyr::transmute(UNITID = .data$UNITID,
                     SURVSECT = "ADM",
                     PART = "H",
                     SATINUM = .data$SATINUM,
                     SATIPCT = .data$SATIPCT,
                     ACTINUM = .data$ACTINUM,
                     ACTIPCT = .data$ACTIPCT,
                     SATVR25 = .data$SATVR25,
                     SATVR75 = .data$SATVR75,
                     SATMT25 = .data$SATMT25,
                     SATMT75 = .data$SATMT75,
                     ACTCM25 = .data$ACTCM25,
                     ACTCM75 = .data$ACTCM75,
                     ACTEN25 = .data$ACTEN25,
                     ACTEN75 = .data$ACTEN75,
                     ACTMT25 = .data$ACTMT25,
                     ACTMT75 = .data$ACTMT75,
                     SATVR50 = .data$SATVR50,
                     SATMT50 = .data$SATMT50,
                     ACTCM50 = .data$ACTCM50,
                     ACTEN50 = .data$ACTEN50,
                     ACTMT50 = .data$ACTMT50)

  return(partH)
}
