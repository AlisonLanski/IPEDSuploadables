#' Make 12 Month Enrollment Part F
#'
#' Flag questions about high school students enrolled for credit
#'
#' @param df A dataframe of student/degree information
#'
#' @importFrom rlang .data
#'
#' @importFrom dplyr select filter distinct
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_e1d_part_F <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

    #Careful: all Dual students need to be HS students as well
    if(sum(df$ISHIGHSCHOOL == 0 & df$ISDUAL == 1) > 0){
      warning(paste0("All students flagged as dual-enrolled must also be flagged as high school; update status for StudentId: ",
                     toString(df$STUDENTID[df$ISHIGHSCHOOL == 0 & df$ISDUAL == 1])))
    }

    partF_prep <- df %>%
      dplyr::select("UNITID",
                    "STUDENTID",
                    "ISHIGHSCHOOL",
                    "ISDUAL") %>%
      dplyr::filter(.data$ISHIGHSCHOOL == 1) %>%
      dplyr::distinct()


    # make final data frame (not counting, just reporting info)
    if(nrow(partF_prep) == 0){
      partF <- data.frame(UNITID = get_ipeds_unitid(df),
                          SURVSECT = "E1D",
                          PART = "F",
                          ENROLLHS = 2,  #no rows = no HS students
                          ENROLLHS_WITHIN = -2,  # these aren't relevant
                          ENROLLHS_OUTSIDE = -2)
    } else {  #we have some!
      partF <- data.frame(UNITID = get_ipeds_unitid(df),
                          SURVSECT = "E1D",
                          PART = "F",
                          ENROLLHS = 1,
                          # works because we filtered to only HS = 1 above
                          ENROLLHS_WITHIN = ifelse(1 %in% partF_prep$ISDUAL, 1, 2),
                          ENROLLHS_OUTSIDE = ifelse(0 %in% partF_prep$ISDUAL, 1, 2))
    }

  return(partF)
}
