#' Create dummy data for testing the completions functions
#'
#' @description Creates a prepared dataframe to test scripts related to IPEDS Completions reporting.
#' Produces either a student/degree dataframe or a dataframe of cips previously reported but not in the current student data,
#' depending on the argument you select
#'
#' @details remember: want to save this data out into the package so it's available
#'
#' @note The final dataset has 100 students with 190 majors.
#' 60 students have 1 major for 1 degree
#' 8 students have 2 majors for 1 degree
#' 17 students have 3 majors (on one degree or as dual-degree)
#' 12 students have 4 majors (as dual-degrees; some have one as an MBA)
#' 3 students have 5 majors (dual-degree and 4 majors and an MBA)
#' Note: 1 student has a faulty birthdate; this will show the warning "1 failed to parse"
#' I use the same seed for each grouping,
#' to ensure that the same students keep getting more things added
#' One program/level combination is flagged as distance education
#' With the seed and probs, no associates degrees end up in the data (don't worry!)
#' To fully process completions, we will need to include an example
#' of a CIP code that is a possible major but has no completers
#' and a CIP code in an award level that is possible but has no completers
#' This is the second piece of dummy df produced
#'
#' @param df_type a string: 'student' to get the main df needed, 'cip' to get extracips
#' @return a dataframe ready for the rest of the comp scripts
#' @export
#' @importFrom dplyr mutate select filter anti_join n
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd dyears
#'

create_dummy_data_com <- function(df_type = 'student'){
  set.seed(1892)
  firstmajors <- data.frame(Unitid = 999999,
                            StudentId = c(100:199),
                            RaceEthnicity = sample(x=c(1:9),
                                                   size = 100,
                                                   replace = T),
                            Sex = sample(x = c(1,2),
                                         size = 100,
                                         replace = T,
                                         prob = c(.47, .53)),
                            DegreeLevel = as.character(sample(x = c(2:8, 17:19),
                                                 size = 100,
                                                 replace = T,
                                                 prob = c(rep(x = .01, 3), .5, .01, .23, .01, .1, .1, .02))),
                            MajorNumber = 1,
                            MajorCip = sample(x = c(09.0100, #journalism
                                                    09.0401,
                                                    09.0701,
                                                    09.0901,
                                                    09.0999,
                                                    09.1001,
                                                    09.9999),
                                              size = 100,
                                              replace = T),
                            DistanceEd = 2,
                            DistanceEd31 = NA,
                            DistanceEd32 = NA,
                            BirthYear = sample(x = c(1977, 1988, 1999, 2002),
                                               size = 100,
                                               replace = T,
                                               prob = c(.05, .1, .80, .05)),
                            BirthMonth = sample(x = c(1:12),
                                                size = 100,
                                                replace = T),
                            BirthDay = sample(x = c(1:31),
                                              size = 100,
                                              replace = T),
                            stringsAsFactors = FALSE) %>%
    dplyr::mutate(Birthdate = lubridate::ymd(paste0(.data$BirthYear, "-",
                                                    .data$BirthMonth, "-",
                                                    ifelse(.data$StudentId == 100, #we need one to fail for testing
                                                           40,  #set day to 40 (DNE)
                                                           .data$BirthDay)))) %>%
    dplyr::select(-c(.data$BirthYear,.data$BirthMonth,.data$BirthDay)) %>%
    #NOTE: this is not an exact Age calculuation -- it doesn't handle leap years.
    # I figured this is good enough for testing the Upload logic
    # To compute age including leap years, use logic that looks at year, then month, than day relative
    # to the arbitrary completions date used below.
    dplyr::mutate(Age = (lubridate::ymd('2018-05-16') - .data$Birthdate)/lubridate::dyears(1))


  ### then for a random subset,
  ### give them a second major in Parks + Rec (same degree level)
  set.seed(1892)
  secondmajors <- firstmajors %>%
    dplyr::filter(.data$StudentId %in% sample(x = .data$StudentId,
                                              size = 50,
                                              replace = T),
                  .data$DegreeLevel %in% c(3, 5, 7, 17, 18, 19)) %>%
    dplyr::mutate(MajorNumber = 2,
                  MajorCip = sample(x = c(31.0101, #parks and rec
                                          31.0301,
                                          31.0399,
                                          31.0504),
                                    size = dplyr::n(),
                                    replace = T))


  ### then for a random subset,
  ### give them an MBA (degree level 7: regardless of the other degree levels they have)
  ### make it distance ed (some programs at level and cip, not all)
  set.seed(1892)
  mba <- firstmajors %>%
    filter(.data$StudentId %in% sample(x = .data$StudentId,
                                       size = 10,
                                       replace = T)) %>%
    mutate(DegreeLevel = "7",
           MajorCip = 52.0201, #general business admin
           DistanceEd = 3,
           DistanceEd31 = 1,
           DistanceEd32 = 0)


  ### then for a random subset,
  ### give them another first major in Engineering (these are dual-degree students)
  set.seed(1892)
  dualfirstdegree <- firstmajors %>%
    dplyr::filter(.data$StudentId %in% sample(x = .data$StudentId,
                                              size = 30,
                                              replace = T)) %>%
    dplyr::mutate(MajorCip = sample(x = c(14.0101, #engineering
                                          14.0702,
                                          14.0799,
                                          14.0801),
                                    size = dplyr::n(),
                                    replace = T))


  ### then for a random subset,
  ### give them another second major in Linguistics (same degree level)
  set.seed(1892)
  anothersecondmajor <- secondmajors %>%
    dplyr::filter(.data$StudentId %in% sample(x = .data$StudentId,
                                              size = 20,
                                              replace = T)) %>%
    dplyr::mutate(MajorCip = sample(x = c(16.0102, #linguistics
                                          16.0104,
                                          16.0105,
                                          16.0199),
                                    size = dplyr::n(),
                                    replace = T),
                  DistanceEd = ifelse(.data$DegreeLevel == 5 & .data$MajorCip == 16.0102,
                                      1,
                                      .data$DistanceEd),
                  DistanceEd31 = NA,
                  DistanceEd32 = NA)



  ### stick all dfs together
  startingdf <- rbind(firstmajors,
                      secondmajors,
                      dualfirstdegree,
                      anothersecondmajor,
                      mba)

  if(tolower(df_type) == 'student'){
    return(startingdf)
  } else {
    allcips <- startingdf %>%
      dplyr::select(.data$MajorCip,
                    .data$DegreeLevel,
                    .data$DistanceEd,
                    .data$DistanceEd31,
                    .data$DistanceEd32) %>%
      unique() %>%
      #add one more at two levels (one level in use, one not in use)
      rbind(data.frame(MajorCip = 45.1001,
                       DegreeLevel = c("1b", "5"),
                       DistanceEd = 1,
                       DistanceEd31 = NA,
                       DistanceEd32 = NA)) %>%
    #add one more with a new cip/level combination that is partial distance ed
        rbind(data.frame(MajorCip=22.0206,
                       DegreeLevel = "1a",
                       DistanceEd = 3,
                       DistanceEd31 = 0,
                       DistanceEd32 = 1))



    extracips <-  allcips %>%
      dplyr::anti_join(startingdf) %>%
      dplyr::mutate(Unitid = 999999,
             RaceEthnicity = 1,
             Sex = 1,
             MajorNumber = 1,
             Count = 0) %>%
      dplyr::select(.data$Unitid,
             .data$MajorNumber,
             .data$MajorCip,
             .data$DegreeLevel,
             .data$DistanceEd,
             .data$DistanceEd31,
             .data$DistanceEd32,
             .data$RaceEthnicity,
             .data$Sex,
             .data$Count)

    return(extracips)
  }
}


#startingdf %>% group_by(StudentId) %>% mutate(Counts = n()) %>% arrange(Counts, StudentId) %>% View()

#startingdf %>% count(StudentId) %>% count(n)



##################################################################################
#############################
##############################
##########  SCRIPT FOR CIPS


#start with all CIPs that made it into our final file;



