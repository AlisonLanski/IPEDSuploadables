#' Create dummy data for testing the Grad Rates 200 function
#'
#' @description Dummy data for Grad Rates 200 testing
#'
#' @param n A number that will be used as the length of the data frame
#'
#' @return a dataframe ready for the rest of the Grad Rates 200 functions
#'
#' @export
#'
#' @examples
#' set.seed(4567)
#' #default creates 1000 students
#' create_dummy_data_gr200()
#' create_dummy_data_gr200(n = 100)
#'

create_dummy_data_gr200 <- function(n = 1000) {

  df <- data.frame(Unitid = 111111,
                   StudentId = seq(from = 1000, to = 999 + n),
                   IsExclusion = sample(0:1, size = n, replace = TRUE, prob = c(.999, .001)),
                   IsComp = sample(0:1, size = n, replace = TRUE, prob = c(.7, .3)),
                   IsStillEnrolled = sample(0:1, size = n, replace = TRUE, prob = c(.1, .9))
                  )

  return(df)
}



