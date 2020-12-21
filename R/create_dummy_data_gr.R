#' Create dummy data for testing the Grad Rates functions
#' @description Creates dummy data for testing the Grad Rates functions
#' @return a dataframe ready for the rest of the Grad Rates functions
#' @export
#'
#'
#'
create_dummy_data_gr <- function(n = 1000, seed = 4567){

  set.seed(seed)
  df <- data.frame(Unitid = 111111,
                   StudentId = seq(from = 1000, to = 999 + n),
                   Section = 1,
                   Line = 1,
                   RaceEthnicity = sample(1:9, size = n, replace = TRUE, prob = c(.1, .1, .03, .2, .2, .03, .2, .1, .04)),
                   Sex = sample(1:2, size = n, replace = TRUE),
                   PellGrant = sample(0:1, size = n, replace = TRUE, prob = c(.3, .7)),
                   DirectLoan = sample(0:1, size = n, replace = TRUE, prob = c(.6, .4))
                  )

  return(df)
}



