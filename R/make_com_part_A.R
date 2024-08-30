#' Make Completions Part A
#'
#' @param df A dataframe of student/degree information
#' @param extracips A dataframe of cips offered by the institution but not in \code{'df'}
#'
#' @importFrom rlang .data
#'
#' @importFrom dplyr select group_by summarize ungroup bind_rows arrange transmute n
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_com_part_A <- function(df, extracips = NULL) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #produce the uploadable format
  partA <- df %>%
           #aggregate the full data
           dplyr::group_by(.data$UNITID,
                           .data$MAJORNUMBER,
                           .data$MAJORCIP,
                           .data$DEGREELEVEL,
                           .data$RACEETHNICITY,
                           .data$SEX) %>%
           dplyr::summarize(COUNT = dplyr::n()) %>%
           dplyr::ungroup()

  #prep the extra cips
  if (!is.null(extracips)) {
    #add extra cips
    colnames(extracips) <- stringr::str_to_upper(colnames(extracips))

    partA <- extracips %>%
             dplyr::select("UNITID",
                           "MAJORNUMBER",
                           "MAJORCIP",
                           "DEGREELEVEL",
                           "RACEETHNICITY",
                           "SEX",
                           "COUNT") %>%
             dplyr::bind_rows(partA)
  }

  #carry on
  partA <- partA %>%
    #sort for easy viewing
    dplyr::arrange(.data$MAJORNUMBER,
                   .data$MAJORCIP,
                   .data$DEGREELEVEL,
                   .data$RACEETHNICITY,
                   .data$SEX) %>%
    #format for upload
    dplyr::transmute(UNITID = .data$UNITID,
                     SURVSECT = "COM",
                     PART = "A",
                     MAJORNUM = .data$MAJORNUMBER,
                     CIPCODE = .data$MAJORCIP,
                     AWLEVEL = .data$DEGREELEVEL,
                     RACE = .data$RACEETHNICITY,
                     SEX = .data$SEX,
                     COUNT = .data$COUNT
                     )

}
