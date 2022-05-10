#' Create dummy data for testing the fall enrollment functions
#'
#' @description Creates students and retention dataframes for use in parts A, B, C, D, E, and G.
#' Student-faculty ratio (part G) will ask for a number when the function is run and does not need to exist here.
#' To create both dataframes, run the function twice with different arguments, and save results into separate objects.
#'
#' @param df_type A string with the dummy data requested ("students" for parts A-D & G or "retention" for part E)
#' @param n A number
#'
#' @return A text file
#'
#' @importFrom rlang .data
#' @importFrom dplyr select group_by summarize ungroup bind_rows arrange transmute n
#' @importFrom utils write.table
#'
#' @export
#'
#' @examples
#' set.seed(1234)
#' #default creates 100 students
#' create_dummy_data_ef1()
#' #change the dataframe
#' create_dummy_data_ef1(df_type = "retention")
#' #change the population size
#' create_dummy_data_ef1(df_type = "students", n = 250)
#'

create_dummy_data_ef1 <- function(df_type = "students", n = 100) {

  if(df_type == "students") {

    df <- data.frame(
      Unitid = 999999,
      StudentId = c(1001:(1000 + n)),
      IsFullTime = sample(0:1, size = n, replace = TRUE),
      StudentLevel = sample(c("Undergraduate", "Graduate"), size = n, replace = TRUE),
      IsDegreeCertSeeking = sample(0:1, size = n, replace = TRUE),
      RaceEthnicity = sample(1:4, size = n, replace = TRUE),
      AdmitState = sample(c(6, 18, 32, 66, 90, 98) , size = n, replace = TRUE),
      Age = sample(c(17, 18, 20, 22, 25, 32, 43, 68),
                   size = n,
                   replace = TRUE,
                   prob = c(.05, .25, .25, .05, .2, .1, .05, .05)),
      Sex = sample(1:2, size = n, replace = TRUE),
      IsFirstTime = sample(0:1, size = n, replace = TRUE),
      DistanceEd = sample(0:2, size = n, replace = TRUE)
    ) %>%
      dplyr::mutate(IsRecentGrad = dplyr::case_when(StudentLevel == "Undergraduate" & IsFirstTime == 1 ~
                                               sample(0:1,
                                                      size = n,
                                                      replace = TRUE,
                                                      prob = c(0.2, 0.8))),
                    IsTransfer = dplyr::case_when(StudentLevel == "Undergraduate" & IsFirstTime == 0 ~
                                             sample(0:1,
                                                    size = n,
                                                    replace = TRUE)),
                    OnlineState = case_when(AdmitState == 66 ~ 6,
                                            TRUE ~ AdmitState)
                    ) %>%
      dplyr::mutate(UnitidState = 6) %>%
      dplyr::mutate(Cip130000 = 0,
                    Cip140000 = 0,
                    Cip260000 = sample(x = c(0:1), size = n, prob = c(.75, .25),  replace = TRUE),
                    Cip270000 = 0,
                    Cip400000 = 0,
                    Cip520000 = sample(x = c(0, 1), size = n, prob = c(.5, .5), replace = TRUE),
                    Cip220101 = 0,
                    Cip510401 = 0,
                    Cip511201 = 0)
  }



    if(df_type == "retention") {

    df <- data.frame(
      Unitid = 999999,
      IsFullTime = c(0, 1),
      OrigCohort = c(500, 1000),
      Exclusions = c(10, 20),
      Inclusions = c(1, 2),
      StillEnrolled = c(450, 925)
    )
  }

  return(df)
}
