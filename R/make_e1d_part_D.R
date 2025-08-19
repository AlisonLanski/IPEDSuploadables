#' Make 12 Month Enrollment Part D
#'
#' @param df A dataframe of student/degree information
#'
#' @importFrom rlang .data
#'
#' @importFrom dplyr select group_by summarize ungroup bind_rows arrange
#'   transmute n
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_e1d_part_D <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partD_counts <- df %>%
    dplyr::select("UNITID",
                  "STUDENTID",
                  "STUDENTLEVEL",
                  "SEX") %>%
    #deduplicate
    dplyr::distinct() %>%
    #aggregate and count
    dplyr::group_by(.data$UNITID,
                    .data$STUDENTLEVEL,
                    .data$SEX) %>%
    dplyr::summarize(COUNT = dplyr::n()) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$STUDENTLEVEL, .data$SEX)


    #set up the final DF
    partD <- data.frame(UNITID = unique(partD_counts$UNITID),
                        SURVSECT = "E1D",
                        PART = "D",
                        FYSEXUG = 0,
                        FYSEXG = 0)

    #ugly way to get the right counts in each bit.
    #I am sure there is a much nicer way to do it with a pivot
    # and maybe with a dummy table for all values. I dunno. This works.

      #if UNK exists
      if(sum(partD_counts$STUDENTLEVEL == 'Undergraduate' &
             partD_counts$SEX != 2 &
             partD_counts$SEX != 1) == 1){
        #then calculate unknown count
        partD_counts$FYSEXUG <- partD_counts$COUNT[partD_counts$STUDENTLEVEL == 'Undergraduate'  &
                                                   partD_counts$SEX != 2 &
                                                   partD_counts$SEX != 1]
      }

      if(sum(partD_counts$STUDENTLEVEL == 'Graduate'&
             partD_counts$SEX != 2 &
             partD_counts$SEX != 1) == 1){
        #then calculate unknown count
        partD_counts$FYSEXG <- partD_counts$COUNT[partD_counts$STUDENTLEVEL == 'Graduate' &
                                                  partD_counts$SEX != 2 &
                                                  partD_counts$SEX != 1]
      }

    return(partD)
}
