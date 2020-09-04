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
#'
#' @return A text file
#' @export
#'


make_com_part_A <- function(df, extracips = NULL, output = "part", format = "both") {


  #produce the uploadable format
  partA <- df %>%
    #aggregate the full data
    dplyr::group_by(.data$Unitid, .data$MajorNumber, .data$MajorCip, .data$DegreeLevel, .data$RaceEthnicity, .data$Sex) %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::ungroup()

  #prep the extra cips
  if (!is.null(extracips)) {
    #add extra cips
    partA <- extracips %>%
      dplyr::select(.data$Unitid, .data$MajorNumber, .data$MajorCip, .data$DegreeLevel, .data$RaceEthnicity, .data$Sex, .data$Count) %>%
      dplyr::bind_rows(partA)
  }

  #carry on
  partA <- partA %>%
    #sort for easy viewing
    dplyr::arrange(.data$MajorNumber, .data$MajorCip, .data$DegreeLevel, .data$RaceEthnicity, .data$Sex) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=COM",
                     PART = "PART=A",
                     MAJORNUM = paste0("MAJORNUM=", .data$MajorNumber),
                     CIPCODE = paste0("CIPCODE=", .data$MajorCip),
                     AWLEVEL = paste0("AWLEVEL=", .data$DegreeLevel),
                     RACE = paste0("RACE=", .data$RaceEthnicity),
                     SEX = paste0("SEX=", .data$Sex),
                     COUNT = paste0("COUNT=", .data$Count)
                     )

  #create the txt file
  write_report(df = partA,
               component = "Completions",
               part = "PartA",
               output = output,
               format = format)
}
