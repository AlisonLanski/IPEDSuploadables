#' Create dummy data for testing the Grad Rates 200 function
#' @description Dummy data for Grad Rates 200 testing
#' @return a dataframe ready for the rest of the Grad Rates 200 functions
#' @export
#'
#'
#'
create_dummy_data_gr200 <- function(n = 1000, seed = 4567) {

  #instructional
  set.seed(seed)

  df <- data.frame(Unitid = 111111,
                   StudentID = seq(from = 1000, to = 999 + n),
                   Is_Exclusion = sample(0:1, size = n, replace = TRUE, prob = c(.999, .001)),
                   Is_Comp = sample(0:1, size = n, replace = TRUE, prob = c(.7, .3)),
                   Is_Still_Enrolled = sample(0:1, size = n, replace = TRUE, prob = c(.1, .9))
                  )

  return(df)
}



