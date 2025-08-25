#' Make Completions Part E
#'
#' @param df A dataframe of student/degree information
#'
#' @importFrom rlang .data
#' @importFrom dplyr select group_by summarize ungroup arrange transmute n distinct
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_com_part_E <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partE_counts <- df %>%
    dplyr::select("UNITID",
                  "STUDENTID",
                  "DEGREELEVEL",
                  "SEX") %>%
    #break into UG and GR levels
    dplyr::mutate(UGPB = ifelse(.data$DEGREELEVEL %in% c(7, 8, 17, 18, 19), 'GR', 'UG')) %>%
    dplyr::select(-"DEGREELEVEL") %>%
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
  partE <- data.frame(UNITID = unique(partE_counts$UNITID),
                      SURVSECT = "COM",
                      PART = "E",
                      CSEXUG = NA,
                      CSEXG = NA)

  #ugly way to get the right counts in each bit.
  #I am sure there is a much nicer way to do it with a pivot
  # and maybe with a dummy table for all values. I dunno. This works.

  #if UNK exists
  if(sum(partE_counts$UGPB == 'UG' &
         partE_counts$SEX != 2 &
         partE_counts$SEX != 1) == 1){
    #then calculate unknown count
    partE$CSEXUG <- partE_counts$COUNT[partE_counts$UGPB == 'UG' &
                                       partE_counts$SEX != 2 &
                                       partE_counts$SEX != 1]
  }

  if(sum(partE_counts$UGPB == 'GR' &
         partE_counts$SEX != 2 &
         partE_counts$SEX != 1) == 1){
    #then calculate unknown count
    partE$CSEXG <- partE_counts$COUNT[partE_counts$UGPB == 'GR' &
                                      partE_counts$SEX != 2 &
                                      partE_counts$SEX != 1]
  }

return(partE)

}



