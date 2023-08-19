#' Create dummy data for testing the completions functions
#'
#' @description Creates a prepared dataframe to test scripts related to IPEDS 12 Month Enrollment reporting.
#' Produces either a student dataframe or a dataframe of instructional activity, depending on the argument you select
#'
#'
#' @note The final dataset has 100 students
#' 60 UG students (40 FT, 20 PT; 26 seeking degrees, 34 not)
#' UG include: 20 first time, 20 transfer, 20 continuing/returning;
#' 40 Grad Students (10 FT, 30 PT; 24 seeking degrees, 16 not)
#'
#' For simplicity, only 1 race-ethnicity category is used
#' 5 UG and 5 Grad are set to be fully distance ed
#' 10 UG are set to be partially distance ed
#'
#' @param df_type a string: "student" to get the main df needed, "instr" to get instructionalactivity
#'
#' @return a dataframe ready for the rest of the e1d scripts
#'
#' @importFrom dplyr mutate filter
#'
#' @export
#'
#' @examples
#' set.seed(1892)
#'
#' student_df <- create_dummy_data_e1d()
#'
#' instr_df <- create_dummy_data_e1d(df_type = "instr")

create_dummy_data_e1d <- function(df_type = "student") {

  students <- data.frame(Unitid = 999999,
                         StudentId = c(100:199),
                         IsFullTime = c(rep(1, 50), rep(0, 50)),
                         IsFirstTime = c(rep(1, 20), rep(0, 80)),
                         IsTransfer = c(rep(0, 20), rep(1, 20), rep(0, 60)),
                         IsDegreeCertSeeking = sample(x = c(0:1),
                                                    size = 100,
                                                    replace = TRUE),
                         StudentLevel = c(rep("Undergraduate", 40), rep("Graduate", 40), rep("Undergraduate", 20)),
                         IsHighSchool = c(rep(0, 99), 1),
                         RaceEthnicity = 1,
                         Sex = sample(x = c(1,2),
                                      size = 100,
                                      replace = TRUE,
                                      prob = c(.47, .53)),
                         DistanceEdAll = c(rep(0, 35), rep(1, 10), rep(0, 55)),
                         DistanceEdSome = c(rep(1, 5), rep(0, 90), rep(1, 5)),
                         stringsAsFactors = FALSE) %>%
    dplyr::mutate(GenderDetail = case_when(StudentId == 179 ~ 4,
                                           StudentId == 180 ~ 4,
                                           StudentId == 181 ~ 3,
                                           TRUE ~ Sex))

  if(tolower(df_type) == "student") {
    return(students)
  } else {
    #placeholder
    hrs <- data.frame(Unitid = 999999,
                      CreditHoursUG = 1000,
                      CreditHoursGR = 500,
                      ClockHoursUG = 0,
                      DocFTE = 8)
    return(hrs)
  }
}

