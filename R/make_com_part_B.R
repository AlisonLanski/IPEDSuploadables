#' Make Completions Part B
#'
#' @param df A dataframe of student/degree information
#' @param extracips A dataframe of cips offered by the institution but not in \code{'df'}
#' @param output A string (\code{"part"}, \code{"full"}, or \code{"both"})
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @importFrom rlang .data
#' @importFrom dplyr select bind_rows arrange transmute
#' @importFrom utils write.table
#'
#' @return A text file
#' @export
#'

make_com_part_B <- function(df, extracips = NULL, output = "part", format = "both") {

  #prep extra cip codes
  if (!is.null(extracips)) {
    extracips_B <- extracips %>%
      select(.data$Unitid, .data$MajorNumber, .data$MajorCip, .data$DegreeLevel, .data$DistanceEd)
  } else {
    extracips_B <- data.frame("Unitid" = NA, "MajorNumber" = NA, "MajorCip" = NA,
                              "DegreeLevel" = NA, "DistanceEd" = NA)
  }

  #prep upload
  partB <- df %>%
    select(.data$Unitid, .data$MajorNumber, .data$MajorCip, .data$DegreeLevel, .data$DistanceEd) %>%
    unique() %>%
    #if we need to add the extra cips, do it here
    bind_rows(extracips_B) %>%
    filter(!is.na(.data$Unitid)) %>%
    #sort for easy viewing
    arrange(.data$MajorNumber, .data$MajorCip, .data$DegreeLevel, .data$DistanceEd) %>%
    #format for upload
    transmute(UNITID = paste0("UNITID=", .data$Unitid),
              SURVSECT = "SURVSECT=COM",
              PART = "PART=B",
              MAJORNUM = paste0("MAJORNUM=", .data$MajorNumber),
              CIPCODE = paste0("CIPCODE=", .data$MajorCip),
              AWLEVEL = paste0("AWLEVEL=", .data$DegreeLevel),
              DistanceED = paste0("DistanceED=", .data$DistanceEd)
              )


  write_report(df = partB,
               component = "Completions",
               part = "PartB",
               output = output,
               format = format)
}
