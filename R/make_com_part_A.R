#' Make Completions Part A
#'
#' @param df A dataframe of student/degree information
#' @param extracips A dataframe of cips offered by the institution but not in \code{'df'}
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by summarize ungroup bind_rows arrange transmute n
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A text file
#' @export
#'

make_com_part_A <- function(df, extracips = NULL, output = "part", format = "both") {

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
             dplyr::select(.data$UNITID,
                           .data$MAJORNUMBER,
                           .data$MAJORCIP,
                           .data$DEGREELEVEL,
                           .data$RACEETHNICITY,
                           .data$SEX,
                           .data$COUNT) %>%
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
    dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                     SURVSECT = "SURVSECT=COM",
                     PART = "PART=A",
                     MAJORNUM = paste0("MAJORNUM=", .data$MAJORNUMBER),
                     CIPCODE = paste0("CIPCODE=", .data$MAJORCIP),
                     AWLEVEL = paste0("AWLEVEL=", .data$DEGREELEVEL),
                     RACE = paste0("RACE=", .data$RACEETHNICITY),
                     SEX = paste0("SEX=", .data$SEX),
                     COUNT = paste0("COUNT=", .data$COUNT)
                     )

  #create the txt file
  write_report(df = partA,
               component = "Completions",
               part = "PartA",
               output = output,
               format = format)
}
