#' Make Admissions Part C (First-time SAT/ACT scores)
#'
#' @description If SAT or ACT scores were used for admission decisions it calculates the requisite percentiles for submission.
#'
#' @param df A dataframe of applicant information
#' @param ptype (Optional) An integer [1-9] indicating which calculation method to use for percentiles. The default value within R and here is 7. To see details, run \code{?quantile} and scroll down to "Type".
#'
#'
#' @importFrom stats quantile
#'
#' @return Admissions Part C data with the required IPEDS structure
#'
#' @export
#'

make_adm_part_C <- function(df, ptype = 7) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  # select first-time students

  partC_SAT_prep <- df %>%
    dplyr::filter(.data$ISFIRSTTIME == 1,
                  .data$ISENROLLED == 1,
                  .data$SATUSED == 1) %>%
    dplyr::select("UNITID",
                  "STUDENTID",
                  "ISENROLLED",
                  "SATUSED",
                  "SAT_EVBRW",
                  "SAT_MATH",
                  )%>%
    dplyr::group_by(.data$UNITID) %>%
    # Add rounding logic
    dplyr::summarize(SATINUM = dplyr::n(),
                     SATVR25 = as.integer(round(quantile(.data$SAT_EVBRW, 0.25, type = ptype)), digits = 0),
                     SATVR50 = as.integer(round(quantile(.data$SAT_EVBRW, 0.50, type = ptype)), digits = 0),
                     SATVR75 = as.integer(round(quantile(.data$SAT_EVBRW, 0.75, type = ptype)), digits = 0),
                     SATMT25 = as.integer(round(quantile(.data$SAT_MATH, 0.25, type = ptype)), digits = 0),
                     SATMT50 = as.integer(round(quantile(.data$SAT_MATH, 0.50, type = ptype)), digits = 0),
                     SATMT75 = as.integer(round(quantile(.data$SAT_MATH, 0.75, type = ptype)), digits = 0)
    )


  #small N or no such students: SAT
  if(nrow(partC_SAT_prep) == 0){
    partC_SAT <- data.frame(UNITID = get_ipeds_unitid(df),
                            SATINUM =  0,
                            SATVR25 = -2,
                            SATVR50 = -2,
                            SATVR75 = -2,
                            SATMT25 = -2,
                            SATMT50 = -2,
                            SATMT75 = -2)
  } else if(partC_SAT_prep$SATINUM < 5){
    partC_SAT <- data.frame(UNITID = get_ipeds_unitid(df),
                            SATINUM = partC_SAT_prep$SATINUM,
                            SATVR25 = -2,
                            SATVR50 = -2,
                            SATVR75 = -2,
                            SATMT25 = -2,
                            SATMT50 = -2,
                            SATMT75 = -2)

  } else {
    partC_SAT <- partC_SAT_prep
  }



  # Calc ACT metrics
  partC_ACT_prep <- df %>%
    dplyr::filter(.data$ISFIRSTTIME == 1,
                  .data$ISENROLLED == 1,
                  .data$ACTUSED == 1) %>%
    dplyr::select("UNITID",
                  "STUDENTID",
                  "ISENROLLED",
                  "ACTUSED",
                  "ACT_ENG",
                  "ACT_COMP",
                  "ACT_MATH")%>%
    dplyr::group_by(.data$UNITID)%>%
    # Add rounding logic
    dplyr::summarize(ACTINUM = dplyr::n(),
                     ACTCM25 = as.integer(round(quantile(.data$ACT_COMP, 0.25, type = ptype)), digits = 0),
                     ACTCM50 = as.integer(round(quantile(.data$ACT_COMP, 0.50, type = ptype)), digits = 0),
                     ACTCM75 = as.integer(round(quantile(.data$ACT_COMP, 0.75, type = ptype)), digits = 0),
                     ACTMT25 = as.integer(round(quantile(.data$ACT_MATH, 0.25, type = ptype)), digits = 0),
                     ACTMT50 = as.integer(round(quantile(.data$ACT_MATH, 0.50, type = ptype)), digits = 0),
                     ACTMT75 = as.integer(round(quantile(.data$ACT_MATH, 0.75, type = ptype)), digits = 0),
                     ACTEN25 = as.integer(round(quantile(.data$ACT_ENG, 0.25, type = ptype)), digits = 0),
                     ACTEN50 = as.integer(round(quantile(.data$ACT_ENG, 0.50, type = ptype)), digits = 0),
                     ACTEN75 = as.integer(round(quantile(.data$ACT_ENG, 0.75, type = ptype)), digits = 0)

    )


  #small N or no such students: ACT
  if(nrow(partC_ACT_prep) == 0){
    partC_ACT <- data.frame(UNITID = get_ipeds_unitid(df),
                            ACTINUM = 0,
                            ACTCM25 = -2,
                            ACTCM50 = -2,
                            ACTCM75 = -2,
                            ACTMT25 = -2,
                            ACTMT50 = -2,
                            ACTMT75 = -2,
                            ACTEN25 = -2,
                            ACTEN50 = -2,
                            ACTEN75 = -2)
  } else if(partC_ACT_prep$ACTINUM < 5){
    partC_ACT <- data.frame(UNITID = get_ipeds_unitid(df),
                            ACTINUM = partC_ACT_prep$ACTINUM,
                            ACTCM25 = -2,
                            ACTCM50 = -2,
                            ACTCM75 = -2,
                            ACTMT25 = -2,
                            ACTMT50 = -2,
                            ACTMT75 = -2,
                            ACTEN25 = -2,
                            ACTEN50 = -2,
                            ACTEN75 = -2)

  } else {
    partC_ACT <- partC_ACT_prep
  }


  # find total first-time for denominator
  ft <- df %>%
    dplyr::filter(.data$ISFIRSTTIME == 1,
                  .data$ISENROLLED == 1) %>%
    dplyr::summarize(COUNT = dplyr::n())

  #get percents
  partC_prep <- dplyr::bind_cols(partC_SAT,
                                 partC_ACT %>% dplyr::select(-"UNITID")
  ) %>%
    dplyr::mutate(SATIPCT = as.integer(round((.data$SATINUM/ft)*100),
                                       digits = 0)) %>%
    dplyr::mutate(ACTIPCT = as.integer(round((.data$ACTINUM/ft)*100),
                                       digits = 0))
  #format for upload
  partC <- partC_prep %>%
    dplyr::transmute(UNITID = .data$UNITID,
                     SURVSECT = "ADM",
                     PART = "C",
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


  return(partC)

}
