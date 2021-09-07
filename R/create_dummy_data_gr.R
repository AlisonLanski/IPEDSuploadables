#' Create dummy data for testing the Grad Rates functions
#' @description Creates dummy data for testing the Grad Rates functions
#' @param n Number of rows of data to synthesize
#' @param seed Seed number to supply to \code{"set.seed()"} to replicate of results
#' @return a dataframe ready for the rest of the Grad Rates functions
#' @export
#'
#'
#'
create_dummy_data_gr <- function(n = 100, seed = 4567) {
  set.seed(seed)
  df <- data.frame(Unitid = 111111,
                   RaceEthnicity = sample(6:8,
                                          size = n,
                                          replace = TRUE,
                                          prob = c(.6, .3, .1)
                                          ),
                   Sex = sample(1:2, size = n, replace = TRUE),
                   ProgramType = sample(1:3,
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
      DirectLoan = dplyr::case_when(PellGrant == 0 ~
                                      sample(0:1,
                                             size = n,
                                             replace = TRUE,
                                             prob = c(.6, .4)
                                             ),
                                    TRUE ~ as.integer(0)
                                    ),

      IsExclusion = dplyr::case_when(IsComp == 0 ~
                                       sample(0:1,
                                              size = n,
                                              replace = TRUE,
                                              prob = c(.8, .2)
                                              ),
                                     TRUE ~ as.integer(0)
                                     ),
      IsTransferOut = dplyr::case_when(IsComp == 0 &
                                         IsExclusion == 0 ~
                                         sample(0:1,
                                                size = n,
                                                replace = TRUE,
                                                prob = c(.8, .2)
                                                ),
                                       TRUE ~ as.integer(0)
                                    ),
      IsStillEnrolled = dplyr::case_when(IsComp == 0 &
                                           IsExclusion == 0 &
                                           IsTransferOut == 0 ~
                                           1,
                                         TRUE ~ 0),

      CompletedFourYears = dplyr::case_when(IsComp == 1 &
                                              ProgramType == 3 ~
                                              sample(0:1,
                                                     size = n,
                                                     replace = TRUE,
                                                     prob = c(.4, .6)
                                                     ),
                                            TRUE ~ as.integer(0)
                                            ),
      CompletedFiveYears = dplyr::case_when(IsComp == 1 &
                                              ProgramType == 3 &
                                              CompletedFourYears == 0 ~
                                              sample(0:1,
                                                     size = n,
                                                     replace = TRUE,
                                                     prob = c(.4, .6)
                                                     ),
                                            TRUE ~ as.integer(0)
                                            ),
      Completed150 = dplyr::case_when(IsComp == 0 ~
                                        0,
                                      ProgramType == 3 ~
                                        1,
                                      ProgramType < 3 ~
                                        as.double(sample(0:1,
                                               size = n,
                                               replace = TRUE,
                                               prob = c(.1, .9)
                                               ))
                                      )
      )


return(df)
}
