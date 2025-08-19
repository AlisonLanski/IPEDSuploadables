#' Make Fall Enrollment Part H
#'
#' @param df A dataframe of student enrollment information
#'
#' @importFrom rlang .data
#' @importFrom dplyr select group_by summarize ungroup arrange transmute n
#'   distinct
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_ef1_part_H <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partH_counts <- df %>%
    dplyr::select("UNITID",
                  "STUDENTID",
                  "STUDENTLEVEL",
                  "SEX"  #Binary = 1, 2
    ) %>%
    #break into UG and GR levels
    dplyr::mutate(UGPB = ifelse(.data$STUDENTLEVEL == 'Graduate', 'GR', 'UG')) %>%
    dplyr::select(-"STUDENTLEVEL") %>%
    #deduplicate
    dplyr::distinct() %>%
    #aggregate and count
    dplyr::group_by(.data$UNITID,
                    .data$UGPB,
                    .data$SEX) %>%
    dplyr::summarize(COUNT = dplyr::n()) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$UGPB, .data$SEX)

  #set up the final DF
  partH <- data.frame(UNITID = unique(partH_counts$UNITID),
                      SURVSECT = "EF1",
                      PART = "H",
                      EFSEXUG = 0,
                      EFSEXG = 0)

  #ugly way to get the right counts in each bit.
  #I am sure there is a much nicer way to do it with a pivot
  # and maybe with a dummy table for all values. I dunno. This works.

  #if UNK exists
  if(sum(partH_counts$UGPB == 'UG' &
         partH_counts$SEX != 2 &
         partH_counts$SEX != 1) == 1){
    #then calculate unknown count
    partH$EFSEXUG <- partH_counts$COUNT[partH_counts$UGPB == 'UG' &
                                         partH_counts$SEX != 2 &
                                         partH_counts$SEX != 1]
  }

  if(sum(partH_counts$UGPB == 'GR' &
         partH_counts$SEX != 2 &
         partH_counts$SEX != 1) == 1){
    #then calculate unknown count
    partH$EFSEXG <- partH_counts$COUNT[partH_counts$UGPB == 'GR' &
                                        partH_counts$SEX != 2 &
                                        partH_counts$SEX != 1]
  }

  return(partH)

}



