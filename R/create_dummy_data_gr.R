#' Create dummy data for testing the Grad Rates functions
#' @description Creates dummy data for testing the Grad Rates functions
#' @param n Number of rows of data to synthesize
#' @param seed Seed number to supply to \code{"set.seed()"} to replicate of results
#' @return a dataframe ready for the rest of the Grad Rates functions
#' @export
#'
#'
#'
create_dummy_data_gr <- function(n = 1000, seed = 4567){

  set.seed(seed)
  df <- data.frame(Unitid = 111111,
                   StudentId = seq(from = 1000, to = 999 + n),
                   IsFullTime = sample(0:1, replace = TRUE),
                   IsFirstTime = sample(0:1, replace = TRUE),
                   IsBachelorsDegreeSeeking = sample(0:1, replace = TRUE),
                   IsExclusion = sample(0:1, replace = TRUE),
                   CompletedLessThanTwoYears = sample(0:1, replace = TRUE),
                   CompletedTwotoFourYears = sample(0:1, replace = TRUE),
                   CompletedFourYears = sample(0:1, replace = TRUE),
                   CompletedFiveYears = sample(0:1, replace = TRUE),
                   IsNonCompleter = sample(0:1, replace = TRUE),
                   IsStillEnrolled = sample(0:1, replace = TRUE),
                   RaceEthnicity = sample(1:9, size = n, replace = TRUE, prob = c(.1, .1, .03, .2, .2, .03, .2, .1, .04)),
                   Sex = sample(1:2, size = n, replace = TRUE),
                   PellGrant = sample(0:1, size = n, replace = TRUE, prob = c(.3, .7)),
                   DirectLoan = sample(0:1, size = n, replace = TRUE, prob = c(.6, .4))
                  )

  return(df)
}



