#' Create dummy data for testing the Grad Rates 200 function
#' @description Dummy data for Grad Rates 200 testing
#' @param n A number that will be used as the length of the data frame
#' @param seed A number to be used in \code{"set.seed()"} for reproducibility
#' @return a dataframe ready for the rest of the Grad Rates 200 functions
#' @export
#'
#'
#'
create_dummy_data_gr200 <- function(n = 1000, seed = 4567) {

  #instructional
  set.seed(seed)

  df <- data.frame(Unitid = 111111,
                   StudentId = seq(from = 1000, to = 999 + n),
                   IsExclusion = sample(0:1, size = n, replace = TRUE, prob = c(.999, .001)),
                   IsComp = sample(0:1, size = n, replace = TRUE, prob = c(.7, .3)),
                   IsStillEnrolled = sample(0:1, size = n, replace = TRUE, prob = c(.1, .9))
                  )

  return(df)
}



