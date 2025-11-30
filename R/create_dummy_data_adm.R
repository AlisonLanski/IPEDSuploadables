#' Create dummy data for testing the Admissions functions
#'
#' @description Creates dummy data for testing the Admissions functions
#'
#' @param seed Number to set a seed to randomize exam scores
#'
#' @return a dataframe of 40 applicants ready for the rest of the Admissions functions
#'
#' @export
#'
#' @examples
#'
#' #use default seed
#' admissions <- create_dummy_data_adm()
#'
#' #use custom seed
#' admissions_scores <- create_dummy_data_adm(seed = 123456)


## this one is much more prescribed than normal - essentially not random
create_dummy_data_adm <- function(seed = 4567) {

  n <- 40  #could set this as an argument, but would need to rewrite most below
  set.seed(seed)
  SATscores = sample(60:80, size = n*2, replace = TRUE)*10
  ACTscores = sample(20:36, size = n*2, replace = TRUE)

  df <- data.frame(Unitid = 111111,
                   StudentId = c(101:140),
                   RaceEthnicity = rep(c(6, 6, 6, 6, 6, 7, 7, 8), 5),
                   Sex = c(1, rep(c(1, 2, 2), 13)),
                   GenderDetail = c(3, rep(c(1, 2, 2), 13)),
                   IsApplicant = 1,
                   IsAdmitted = rep(c(1, 1, 1, 1, 0, 1, 1, 0), 5),
                   IsEnrolled = rep(c(1, 1, 1, 0, 0, 1, 0, 0), 5),
                   IsFullTime = rep(c(1, 1, 0, 1, 0), 8),
                   IsFirstTime = rep(c(1, 1, 1, 0, 1), 8),
                   IsTransfer = rep(c(0, 0, 0, 1, 0), 8),
                   SATUsed =  rep(c(1, 1, 1, 1, 0, 0, 0, 0), 5),
                   ACTUsed = rep(c(0, 0, 0, 1, 1, 0, 0, 0), 5),
                   SAT_Evbrw = SATscores[1:n],
                   SAT_Math = SATscores[(n+1):(n*2)],
                   ACT_Comp = floor((ACTscores[1:n] + ACTscores[(n+1):(n*2)])/2),
                   ACT_Eng = ACTscores[1:n],
                   ACT_Math = ACTscores[(n+1):(n*2)]) %>%
    dplyr::mutate(SAT_Evbrw = ifelse(.data$StudentId >= 130 & .data$SATUsed == 0, NA, .data$SAT_Evbrw),
                  SAT_Math = ifelse(is.na(.data$SAT_Evbrw), NA, .data$SAT_Math))

  return(df)
}
