#' Make Completions Part B
#'
#' @param df
#' @param extracips
#'
#' @importFrom rlang .data
#' @importFrom dplyr select bind_rows arrange transmute
#'
#' @return
#' @export
#'
#' @examples
make_com_part_B <- function(df, extracips = NULL) {

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

  #just this part
  write.table(x = partB, sep = ",",
              file = paste0(path, "Completions_PartB_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  #append to the upload doc
  write.table(x = partB, sep = ",",
              file = paste0(path, "Completions_PartsAll_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
}
