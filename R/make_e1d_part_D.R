#' Make 12 Month Enrollment Part D (gender details)
#'
#' @param df A dataframe of student/degree information
#' @param ugender A boolean: TRUE means you are collecting and able to report
#'   "another gender" for undergraduate students, even if you have no (or few)
#'   such students. Set as FALSE if necessary
#' @param ggender A boolean: TRUE means you are collecting and able to report
#'   "another gender" for graduate students, even if you have no (or few) such
#'   students. Set as FALSE if necessary
#'
#' @importFrom rlang .data
#'
#' @importFrom dplyr select group_by summarize ungroup bind_rows arrange
#'   transmute n
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A text file
#' @export
#'

make_e1d_part_D <- function(df, ugender, ggender) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partD_counts <- df %>%
    dplyr::select(.data$UNITID,
                  .data$STUDENTID,
                  .data$STUDENTLEVEL,
                  .data$GENDERDETAIL) %>%
    #deduplicate
    dplyr::distinct() %>%
    #aggregate and count
    dplyr::group_by(.data$UNITID,
                    .data$STUDENTLEVEL,
                    .data$GENDERDETAIL) %>%
    dplyr::summarize(COUNT = dplyr::n()) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$STUDENTLEVEL, .data$GENDERDETAIL)


    #set up the final DF
    partD <- data.frame(UNITID = unique(partD_counts$UNITID),
                        SURVSECT = "E1D",
                        PART = "D",
                        FYGU01 = 0,  #UG detail reporting?
                        FYGU011 = 0, #UG Unknown
                        FYGU012 = 0, #UG Another
                        FYGU02 = 0,  #GR detail reporting?
                        FYGU021 = 0, #GR Unknown
                        FYGU022 = 0) #GR Another

    #ugly way to get the right counts in each bit.
    #I am sure there is a much nicer way to do it with a pivot
    # and maybe with a dummy table for all values. I dunno. This works.

    # #No UG in the data
    if(!'Undergraduate' %in% partD_counts$STUDENTLEVEL){
      partD$FYGU01 <- 2  #not reporting another
      partD$FYGU012 <- -2  #not applicable

      #  #UG in the data, another not being reported, unknown might exist
    } else if(ugender == FALSE){
      partD$FYGU01 <- 2
      if(sum(partD_counts$STUDENTLEVEL == 'Undergraduate' &
             partD_counts$GENDERDETAIL == 3) == 1){
        partD$FYGU011 <- partD_counts$COUNT[partD_counts$STUDENTLEVEL == 'Undergraduate' &
                                             partD_counts$GENDERDETAIL == 3]
      }
      partD$FYGU012 <- -2

      # #UG in the data, another being reported, another/unknown might or might not exist
    } else {
      partD$FYGU01 <- 1

      if(sum(partD_counts$STUDENTLEVEL == 'Undergraduate' &
             partD_counts$GENDERDETAIL == 3) == 1){
        partD$FYGU011 <- partD_counts$COUNT[partD_counts$STUDENTLEVEL == 'Undergraduate' &
                                             partD_counts$GENDERDETAIL == 3]
      }
      if(sum(partD_counts$STUDENTLEVEL == 'Undergraduate' &
             partD_counts$GENDERDETAIL == 4) == 1){
        partD$FYGU012 <- partD_counts$COUNT[partD_counts$STUDENTLEVEL == 'Undergraduate' &
                                             partD_counts$GENDERDETAIL == 4]
        #BUT -- New in 2023 - mask if < 5 and set initial inquiry as "small N"
        if(partD$FYGU012 < 5){
          partD$FYGU012 <- -2
          partD$FYGU01 <- 3
        }
      }
    }

    # #No GR in the data
    if(!'Graduate' %in% partD_counts$STUDENTLEVEL){
      partD$FYGU02 <- 2  #not reporting another
      partD$FYGU022 <- -2  #not applicable

      #  #GR in the data, another not being reported, unknown might exist
    } else if(ggender == FALSE){
      partD$FYGU02 <- 2
      if(sum(partD_counts$STUDENTLEVEL == 'Graduate' &
             partD_counts$GENDERDETAIL == 3) == 1){
        partD$FYGU021 <- partD_counts$COUNT[partD_counts$STUDENTLEVEL == 'Graduate' &
                                             partD_counts$GENDERDETAIL == 3]
      }
      partD$FYGU022 <- -2

      # #GR in the data, another being reported, another/unknown might or might not exist
    } else {
      partD$FYGU02 <- 1

      if(sum(partD_counts$STUDENTLEVEL == 'Graduate' &
             partD_counts$GENDERDETAIL == 3) == 1){
        partD$FYGU021 <- partD_counts$COUNT[partD_counts$STUDENTLEVEL == 'Graduate' &
                                             partD_counts$GENDERDETAIL == 3]
      }
      if(sum(partD_counts$STUDENTLEVEL == 'Graduate' &
             partD_counts$GENDERDETAIL == 4) == 1){
        partD$FYGU022 <- partD_counts$COUNT[partD_counts$STUDENTLEVEL == 'Graduate' &
                                             partD_counts$GENDERDETAIL == 4]
        #BUT -- New in 2023 - mask if < 5 and set initial inquiry as "small N"
        if(partD$FYGU022 < 5){
          partD$FYGU022 <- -2
          partD$FYGU02 <- 3
        }
      }
    }

    return(partD)
}
