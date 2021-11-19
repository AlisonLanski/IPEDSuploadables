#' Make Completions Part B
#'
#' @param df A dataframe of student/degree information
#' @param extracips A dataframe of cips offered by the institution but not in \code{'df'}
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom dplyr select bind_rows arrange transmute distinct
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A text file
#' @export
#'

make_com_part_B <- function(df, extracips = NULL, output = "part", format = "both") {

  #prep extra cip codes
  if (!is.null(extracips)) {
    colnames(extracips) <- stringr::str_to_upper(colnames(extracips))

    extracips_B <- extracips %>%
      dplyr::select(.data$UNITID, .data$MAJORNUMBER, .data$MAJORCIP, .data$DEGREELEVEL, .data$DISTANCEED, .data$DISTANCEED31, .data$DISTANCEED32)
  } else {
    extracips_B <- data.frame("UNITID" = NA, "MAJORNUMBER" = NA, "MAJORCIP" = NA,
                              "DEGREELEVEL" = NA, "DISTANCEED" = NA, "DISTANCEED31" = NA, "DISTANCEED32" = NA)
  }

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #prep upload
  partB <- df %>%
    dplyr::select(.data$UNITID, .data$MAJORNUMBER, .data$MAJORCIP, .data$DEGREELEVEL, .data$DISTANCEED, .data$DISTANCEED31, .data$DISTANCEED32) %>%
    dplyr::distinct() %>%
    #if we need to add the extra cips, do it here
    dplyr::bind_rows(extracips_B) %>%
    dplyr::filter(!is.na(.data$UNITID)) %>%
    #sort for easy viewing
    dplyr::arrange(.data$MAJORNUMBER, .data$MAJORCIP, .data$DEGREELEVEL, .data$DISTANCEED, .data$DISTANCEED31, .data$DISTANCEED32) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$UNITID),
                    SURVSECT = "SURVSECT=COM",
                    PART = "PART=B",
                    MAJORNUM = paste0("MAJORNUM=", .data$MAJORNUMBER),
                    CIPCODE = paste0("CIPCODE=", .data$MAJORCIP),
                    AWLEVEL = paste0("AWLEVEL=", .data$DEGREELEVEL),
                    #2020 is non-rectangular: this was the best solution I could think of
                    DistanceED = ifelse(.data$DISTANCEED != 3,
                                    paste0("DistanceED=", .data$DISTANCEED),
                                    paste0("DistanceED=", .data$DISTANCEED,
                                           ",DistanceED31=", .data$DISTANCEED31,
                                           ",DistanceED32=", .data$DISTANCEED32))
                    )


  write_report(df = partB,
               component = "Completions",
               part = "PartB",
               output = output,
               format = format)
}
