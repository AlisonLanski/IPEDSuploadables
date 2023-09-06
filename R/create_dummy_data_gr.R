#' Create dummy data for testing the Grad Rates functions
#'
#' @description Creates dummy data for testing the Grad Rates functions
#'
#' @param n Number of rows of data to synthesize
#'
#' @return a dataframe ready for the rest of the Grad Rates functions
#'
#' @export
#'
#' @examples
#' #use this seed to reproduce the dummy data saved to the package
#' set.seed(4567)
#'
#' #default makes 100 students
#' graduated <- create_dummy_data_gr()
#'
#' more_graduated <- create_dummy_data_gr(n = 500)

create_dummy_data_gr <- function(n = 100) {

  df <- data.frame(Unitid = 111111,
                   RaceEthnicity = sample(6:8,
                                          size = n,
                                          replace = TRUE,
                                          prob = c(.6, .3, .1)
                                          ),
                   Sex = sample(1:2, size = n, replace = TRUE),
                   EnteringProgramType = sample(1:3,
                                        size = n,
                                        replace = TRUE,
                                        prob = c(.2, .1, .7)
                                        ),
                   IsComp = c(rep(1, n * .75),
                              rep(0, n * .25)
                              ),
                   PellGrant = sample(0:1,
                                      size = n,
                                      replace = TRUE,
                                      prob = c(.3, .7)
                                      )
                   ) %>%
    dplyr::mutate(
      DirectLoan = dplyr::case_when(.data$PellGrant == 0 ~
                                      sample(0:1,
                                             size = n,
                                             replace = TRUE,
                                             prob = c(.6, .4)
                                             ),
                                    TRUE ~ as.integer(0)
                                    ),

      IsExclusion = dplyr::case_when(.data$IsComp == 0 ~
                                       sample(0:1,
                                              size = n,
                                              replace = TRUE,
                                              prob = c(.8, .2)
                                              ),
                                     TRUE ~ as.integer(0)
                                     ),
      IsTransferOut = dplyr::case_when(.data$IsComp == 0 &
                                         .data$IsExclusion == 0 ~
                                         sample(0:1,
                                                size = n,
                                                replace = TRUE,
                                                prob = c(.8, .2)
                                                ),
                                       TRUE ~ as.integer(0)
                                    ),
      IsStillEnrolled = dplyr::case_when(.data$IsComp == 0 &
                                           .data$IsExclusion == 0 &
                                           .data$IsTransferOut == 0 ~
                                           1,
                                         TRUE ~ 0),
      CurrentProgramType = .data$EnteringProgramType,
      CompletedFourYears = dplyr::case_when(.data$IsComp == 1 &
                                              .data$EnteringProgramType == 3 ~
                                              sample(0:1,
                                                     size = n,
                                                     replace = TRUE,
                                                     prob = c(.4, .6)
                                                     ),
                                            TRUE ~ as.integer(0)
                                            ),
      CompletedFiveYears = dplyr::case_when(.data$IsComp == 1 &
                                              .data$EnteringProgramType == 3 &
                                              .data$CompletedFourYears == 0 ~
                                              sample(0:1,
                                                     size = n,
                                                     replace = TRUE,
                                                     prob = c(.4, .6)
                                                     ),
                                            TRUE ~ as.integer(0)
                                            ),
      Completed150 = dplyr::case_when(.data$IsComp == 0 ~
                                        0,
                                      .data$EnteringProgramType == 3 ~
                                        1,
                                      .data$EnteringProgramType < 3 ~
                                        as.double(sample(0:1,
                                               size = n,
                                               replace = TRUE,
                                               prob = c(.1, .9)
                                               ))
                                      )
            ) %>%
    #add 1 person who switches from 2-4 yr to BA
    rbind(data.frame(Unitid = 111111,
          RaceEthnicity = 1,
          Sex = 1,
          EnteringProgramType = 2,
          IsComp = 1,
          PellGrant = 1,
          DirectLoan = 1,
          IsExclusion = 0,
          IsTransferOut = 0,
          IsStillEnrolled = 0,
          CurrentProgramType = 3,
          CompletedFourYears = 1,
          CompletedFiveYears = 0,
          Completed150 = 1
          )) %>%
    dplyr::select(-"IsComp") %>%
    #we need some folks with unknown or another gender
    dplyr::mutate(GenderDetail = c(utils::head(.data$Sex,-6), 3, 3, 3, 3, 4, 4), .after = .data$Sex)


  return(df)
}
