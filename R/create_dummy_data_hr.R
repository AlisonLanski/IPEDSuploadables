#' Create dummy data for testing the hr functions
#' @description to do: save this out into the package and make it accessible as package data
#' @return a dataframe ready for the rest of the hr scripts
#' @export
#'
#'
#'
create_dummy_data_hr <- function(){
  n = 1000
  m = 100
  p = 2000
  q = 500

  #instructional
  set.seed(4567)
  instructional <- data.frame(Unitid = 111111,
                              EmpId = seq(from = 1000, to = 999+n),
                              NewHire = sample(0:1, size = n, replace = TRUE, prob = c(.9, .1)),
                              #add a few non-current later, including new hires
                              CurrentEmployee = 1,
                              IsMedical = sample(0:1, size = n, replace = T, prob = c(.8, .2)),
                              RaceEthnicity = sample(1:9, size = n, replace = TRUE, prob = c(.1, .1, .03, .2, .2, .03, .2, .1, .04)),
                              Gender = sample(1:2, size = n, replace = TRUE),
                              FtPt = sample(c('F', 'P'), size = n, replace = TRUE, prob = c(.75, .25)),
                              Salary = sample(50000:500000, size = n, replace = T),
                              OccCategory3 = sample(c(1:4), size = n, replace = T,
                                                    prob = c(.3, .1, .1, .5)),
                              Months = sample(c(8:12), size = n, replace = T, prob= c(.1, .7, .1, 0, .1)),
                              Rank = sample(1:6, size = n, replace = T, prob = c(.3, .2, .2, .1, .05, .15)),
                              Tenure = sample(1:6, size = n, replace = T, prob = c(.3, .2, .2, .2, .05, .05)))


  #other faculty
  faculty <- data.frame(Unitid = 111111,
                        EmpId = seq(from = 10000, to = 9999+m),
                        NewHire = sample(0:1, size = m, replace = TRUE, prob = c(.9, .1)),
                        #add a few non-current later, including new hires
                        CurrentEmployee = 1,
                        IsMedical = sample(0:1, size = m, replace = T, prob = c(.8, .2)),
                        RaceEthnicity = sample(1:9, size = m, replace = TRUE, prob = c(.1, .1, .03, .2, .2, .03, .2, .1, .04)),
                        Gender = sample(1:2, size = m, replace = TRUE),
                        FtPt = sample(c('F', 'P'), size = m, replace = TRUE, prob = c(.75, .25)),
                        Salary = sample(50000:500000, size = m, replace = T),
                        OccCategory3 = sample(c(5:20), size = m, replace = T,
                                              prob = c(.3, .2, .1, 0, 0, .05, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
                        Months = sample(c(8:12), size = m, replace = T, prob= c(.01, .01, .01, 01, .96)),
                        Rank = sample(1:6, size = m, replace = T, prob = c(.3, .2, .2, .1, .05, .15)),
                        Tenure = sample(1:5, size = m, replace = T, prob = c(.3, .2, .2, .2, .1)))

  #staff
  staff <- data.frame(Unitid = 111111,
                      EmpId = seq(from = 100000, to = 99999+p),
                      NewHire = sample(0:1, size = p, replace = TRUE, prob = c(.9, .1)),
                      #add a few non-current later, including new hires
                      CurrentEmployee = 1,
                      IsMedical = sample(0:1, size = p, replace = T, prob = c(.8, .2)),
                      RaceEthnicity = sample(1:9, size = p, replace = TRUE, prob = c(.1, .1, .03, .2, .2, .03, .2, .1, .04)),
                      Gender = sample(1:2, size = p, replace = TRUE),
                      FtPt = sample(c('F', 'P'), size = p, replace = TRUE, prob = c(.75, .25)),
                      Salary = sample(15000:500000, size = p, replace = T),
                      OccCategory3 = sample(c(5:20), size = p, replace = T,
                                            prob = c(.03, .03, .01, .05,
                                                     .05, .09, .09, .09,
                                                     .09, .05, .09, .09,
                                                     .05, .09, .05, .05)),
                      Months = sample(c(8:12), size = p, replace = T, prob= c(.01, .01, .01, 01, .96)),
                      Rank = 6,
                      Tenure = 7)


  #grad students
  grad <- data.frame(Unitid = 111111,
                     EmpId = seq(from = 1000000, to = 999999+q),
                     NewHire = 0,
                     #add a few non-current later, including new hires
                     CurrentEmployee = 1,
                     IsMedical = sample(0:1, size = q, replace = T, prob = c(.8, .2)),
                     RaceEthnicity = sample(1:9, size = q, replace = TRUE, prob = c(.1, .1, .03, .2, .2, .03, .2, .1, .04)),
                     Gender = sample(1:2, size = q, replace = TRUE),
                     FtPt = 'P',
                     Salary = NA,
                     OccCategory3 = sample(22:24, size = q, replace = T,
                                           prob = c(0.5, .4, .1)),
                     Months = 99,
                     Rank = 6,
                     Tenure = 7)


  ipeds_df <- rbind(instructional, faculty, staff, grad)

  return(ipeds_df)

}



