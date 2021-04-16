#' Make Fall Enrollment Dummy Data
#'
#' @param df A dataframe of student/degree information
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by summarize ungroup bind_rows arrange transmute n
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'

create_dummy_data_ef1 <- function(n = 100, seed = 1234) {

  set.seed(seed)

  df <- data.frame(
          Unitid = 999999,
          MajorCip = sample(x = c(09.0100, #journalism
                                  09.0401,
                                  09.0701,
                                  09.0901,
                                  09.0999,
                                  09.1001,
                                  09.9999),
                            size = n,
                            replace = TRUE),
          IsFullTime = sample(0:1, size = n, replace = TRUE),
          IsUgrd = sample(0:1, size = n, replace = TRUE),
          IsDegreeSeeking = sample(0:1, size = n, replace = TRUE),
          RaceEthnicity = sample(1:9, size = n, replace = TRUE),
          StateOfResidence = sample(1:90, size = n, replace = TRUE),
          Age = sample(10:100, size = n, replace = TRUE),
          Sex = sample(1:2, size = n, replace = TRUE),
          IsFirstTime = sample(0:1, size = n, replace = TRUE)
  )

  return(df)
}
