#' Create dummy data for testing the completions functions
#'
#' @description Creates a prepared dataframe to test scripts related to IPEDS Completions reporting.
#' Produces either a student/degree dataframe or a dataframe of cips previously reported but not in the current student data,
#' depending on the argument you select
#'
#' @note The final dataset has 60 students with 105 majors.
#' Students 100-130, 140, 150 have 1 major for 1 degree (journalism)
#' Students 131-139 have 2 majors for 1 degree (journalism + parks)
#' Students 141-149 have 3 majors for 1 degree (journalism, parks, linguistics)
#' Students 151-159 have 3 majors for 2 degrees (1 degree with journalism/parks, 1 MBA degree)
#' Note: 1 student has a faulty birthdate; this will show the warning "1 failed to parse"
#'
#' Two rows (level 18 linguistics) are flagged as distance education
#'
#' To fully process completions, we will need to include an example
#' of a CIP code that is a possible major but has no completers
#' and a CIP code in an award level that is possible but has no completers
#' This is the second piece of dummy df produced
#'
#' @param df_type a string: "student" to get the main df needed, "cip" to get extracips
#'
#' @return a dataframe ready for the rest of the comp scripts

#' @importFrom dplyr mutate select filter anti_join n
#' @importFrom lubridate ymd dyears
#'
#' @export
#'
#' @examples
#' set.seed(1892)
#'
#' # one date fails to parse:
#' # this is to provide an example of missing
#' # data which is acceptable to IPEDS
#' students <- create_dummy_data_com()
#'
#' additional_cips <- create_dummy_data_com(df_type = "cip")

create_dummy_data_com <- function(df_type = "student") {


  firstmajors <- data.frame(Unitid = 999999,
                            StudentId = c(100:199),
                            RaceEthnicity = rep(x = c(1,2,8,9), 25),
                            Sex = rep(x = c(1, 1, 2, 2, 2), 20),
                            DegreeLevel = as.character(rep(c(2, 3, 3, 5, 5, 5, 5, 5, 5, 5,
                                                             6, 7, 7, 7, 7, 17, 17, 18, 18, 19)),
                                                       5),
                            MajorNumber = 1,
                            MajorCip = rep(x = c(09.0100, #journalism
                                                 09.9999),
                                              50),
                            DistanceEd = 2,
                            DistanceEd31 = NA,
                            DistanceEd32 = NA,
                            BirthYear = c(rep(2002, 5),
                                          rep(1999, 80),
                                          rep(1998,10),
                                          rep(1977, 5)),
                            BirthMonth = c(rep(3,50), rep(10, 50)),
                            BirthDay = rep(c(15, 20), 50),
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
    dplyr::mutate(Age = (lubridate::ymd('2018-05-16') - .data$Birthdate)/lubridate::dyears(1)) %>%
    #adding more detailed sex info for 2022+ reporting years
    dplyr::mutate(GenderDetail = dplyr::case_when(.data$StudentId < 103 ~ 3, #unknown UG (3)
                                               .data$StudentId < 105 ~ 4, #another UG (2)
                                               .data$StudentId > 154 ~ 4, #another GR (5)
                                               TRUE ~ .data$Sex))


  ### then for a subset,
  ### give them a second major in Parks + Rec (same degree level)
  secondmajors <- firstmajors %>%
    dplyr::filter(.data$StudentId >= 131 & .data$StudentId < 160,
                  .data$DegreeLevel %in% c(3, 5, 7, 17, 18, 19)) %>%
    dplyr::mutate(MajorNumber = 2,
                  MajorCip = 31.0101) #parks and rec



  ### then for a subset,
  ### give them an MBA (degree level 7: regardless of the other degree levels they have)
  ### make it distance ed (some programs at level and cip, not all)
  mba <- firstmajors %>%
    dplyr::filter(.data$StudentId >= 151 & .data$StudentId <= 159) %>%
    dplyr::mutate(DegreeLevel = "7",
           MajorCip = 52.0201, #general business admin
           DistanceEd = 3,
           DistanceEd31 = 1,
           DistanceEd32 = 0)


  ### then for a random subset,
  ### give them another second major in Linguistics (same degree level)
  anothersecondmajor <- secondmajors %>%
    dplyr::filter(.data$StudentId %in% c(141:149)) %>%
    dplyr::mutate(MajorCip = 16.0102, #linguistics
                  DistanceEd = ifelse(.data$DegreeLevel == 3,
                                      1,
                                      .data$DistanceEd),
                  DistanceEd31 = NA,
                  DistanceEd32 = NA)

  #remove extra students
  firstmajors <- firstmajors %>% dplyr::filter(.data$StudentId < 160)

  ### stick all dfs together
  startingdf <- rbind(firstmajors,
                      secondmajors,
                      anothersecondmajor,
                      mba)

  if(tolower(df_type) == "student") {
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
        rbind(data.frame(MajorCip = 22.0206,
                         DegreeLevel = "1a",
                         DistanceEd = 3,
                         DistanceEd31 = 0,
                         DistanceEd32 = 1))



    extracips <- allcips %>%
      dplyr::anti_join(startingdf, by = c("MajorCip",
                                          "DegreeLevel",
                                          "DistanceEd",
                                          "DistanceEd31",
                                          "DistanceEd32")) %>%
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
