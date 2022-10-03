#' Make Completions Part E (gender details)
#'
#' @param df A dataframe of student/degree information
#' @param ugender A boolean: TRUE means you are collecting and able to report
#'   "another gender" for undergraduate completers. Set as FALSE if necessary
#' @param ggender A boolean: TRUE means you are collecting and able to report
#'   "another gender" for graduate completers. Set as FALSE if necessary
#'
#' @importFrom rlang .data
#' @importFrom dplyr select group_by summarize ungroup arrange transmute n distinct
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A text file
#' @export
#'

make_com_part_E <- function(df, ugender, ggender) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partE_counts <- df %>%
    dplyr::select(.data$UNITID,
                  .data$STUDENTID,
                  .data$DEGREELEVEL,
                  .data$GENDERDETAIL  #Binary = 1, 2;  Unknown = 3, Another = 4
                  ) %>%
    #break into UG and GR levels
    dplyr::mutate(UGPB = ifelse(.data$DEGREELEVEL %in% c(7, 8, 17, 18, 19), 'GR', 'UG')) %>%
    dplyr::select(-.data$DEGREELEVEL) %>%
    #deduplicate
    dplyr::distinct() %>%
    #aggregate and count
    dplyr::group_by(.data$UNITID,
                    .data$UGPB,
                    .data$GENDERDETAIL) %>%
    dplyr::summarize(COUNT = dplyr::n()) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$UGPB, .data$GENDERDETAIL)

  #set up the final DF
  partE <- data.frame(UNITID = unique(partE_counts$UNITID),
                           SURVSECT = "COM",
                           PART = "E",
                           CGU01 = 0,  #UG detail reporting?
                           CGU011 = 0, #UG Unknown
                           CGU012 = 0, #UG Another
                           CGU02 = 0,  #GR detail reporting?
                           CGU021 = 0, #GR Unknown
                           CGU022 = 0) #GR Another

  #ugly way to get the right counts in each bit.
  #I am sure there is a much nicer way to do it with a pivot
  # and maybe with a dummy table for all values. I dunno. This works.

  # #No UG in the data
  if(!'UG' %in% partE_counts$UGPB){
    partE$CGU01 <- 2  #not reporting another
    partE$CGU012 <- -2  #not applicable

  #  #UG in the data, another not being reported, unknown might exist
  } else if(ugender == FALSE){
    partE$CGU01 <- 2
    if(sum(partE_counts$UGPB == 'UG' &
           partE_counts$GENDERDETAIL == 3) == 1){
      partE$CGU011 <- partE_counts$COUNT[partE_counts$UGPB == 'UG' &
                                           partE_counts$GENDERDETAIL == 3]
    }
    partE$CGU012 <- -2

  # #UG in the data, another being reported, another/unknown might or might not exist
  } else {
    partE$CGU01 <- 1

    if(sum(partE_counts$UGPB == 'UG' &
           partE_counts$GENDERDETAIL == 3) == 1){
      partE$CGU011 <- partE_counts$COUNT[partE_counts$UGPB == 'UG' &
                                           partE_counts$GENDERDETAIL == 3]
    }
    if(sum(partE_counts$UGPB == 'UG' &
           partE_counts$GENDERDETAIL == 4) == 1){
      partE$CGU012 <- partE_counts$COUNT[partE_counts$UGPB == 'UG' &
                                           partE_counts$GENDERDETAIL == 4]
    }
  }

  # #No GR in the data
  if(!'GR' %in% partE_counts$UGPB){
    partE$CGU02 <- 2  #not reporting another
    partE$CGU022 <- -2  #not applicable

    #  #GR in the data, another not being reported, unknown might exist
  } else if(ggender == FALSE){
    partE$CGU02 <- 2
    if(sum(partE_counts$UGPB == 'GR' &
           partE_counts$GENDERDETAIL == 3) == 1){
      partE$CGU021 <- partE_counts$COUNT[partE_counts$UGPB == 'GR' &
                                           partE_counts$GENDERDETAIL == 3]
    }
    partE$CGU022 <- -2

    # #GR in the data, another being reported, another/unknown might or might not exist
  } else {
    partE$CGU02 <- 1

      if(sum(partE_counts$UGPB == 'GR' &
             partE_counts$GENDERDETAIL == 3) == 1){
        partE$CGU021 <- partE_counts$COUNT[partE_counts$UGPB == 'GR' &
                                           partE_counts$GENDERDETAIL == 3]
      }
      if(sum(partE_counts$UGPB == 'GR' &
             partE_counts$GENDERDETAIL == 4) == 1){
        partE$CGU022 <- partE_counts$COUNT[partE_counts$UGPB == 'GR' &
                                           partE_counts$GENDERDETAIL == 4]
      }
    }

return(partE)

}



