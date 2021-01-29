#' Create dummy data for testing the outcome measures functions
#'
#' @description Creates a prepared dataframe to test scripts related to IPEDS Outcome Measures reporting.
#' Produces either a student/status dataframe
#'
#' @details remember: want to save this data out into the package so it's available
#'
#' @note The final dataset has 20 students covering most statuses
#'
#' @return a dataframe ready for the rest of the om scripts
#' @export
#'

create_dummy_data_om <- function(){
  om_dat <- data.frame(Unitid = 999999,
                            StudentId = c(100:119),
                            CohortType = c(rep(1, 10), rep(2, 3), rep(3, 7)),
                            Recipient = c(rep(1, 5), rep(2, 15)),
                            Exclusion = c(rep(FALSE, 16), rep(TRUE, 2), rep(FALSE, 2)),
                            AwardLevel4 = c(rep(2, 5), rep(3, 8), rep(4, 7)),
                            AwardLevel6 = c(rep(2, 5), rep(3, 10), rep(4, 5)),
                            AwardLevel8 = c(rep(2, 5), rep(3, 11), rep(4, 4)),
                            EnrollStatus8 = c(rep(4, 18), rep(1, 1), rep(2, 1)),
                            stringsAsFactors = FALSE)

    return(om_dat)
}
