#' Make Completions Part B
#'
#' @param df
#' @param extracips
#'
#' @return
#' @export
#'
#' @examples
make_com_part_B <- function(df, extracips = NULL) {

  #prep extra cip codes
  if (!is.null(extracips)) {
    extracips_B <- extracips %>%
      select(Unitid, MajorNumber, MajorCip, DegreeLevel, DistanceEd)
  } else {
    extracips_B <- data.frame("Unitid" = NA, "MajorNumber" = NA, "MajorCip" = NA,
                              "DegreeLevel" = NA, "DistanceEd" = NA)
  }

  #prep upload
  partB <- df %>%
    select(Unitid, MajorNumber, MajorCip, DegreeLevel, DistanceEd) %>%
    unique() %>%
    #if we need to add the extra cips, do it here
    bind_rows(extracips_B) %>%
    #sort for easy viewing
    arrange(MajorNumber, MajorCip, DegreeLevel, DistanceEd) %>%
    #format for upload
    transmute(UNITID = paste0("UNITID=", Unitid),
              SURVSECT = "SURVSECT=COM",
              PART = "PART=B",
              MAJORNUM = paste0("MAJORNUM=", MajorNumber),
              CIPCODE = paste0("CIPCODE=", MajorCip),
              AWLEVEL = paste0("AWLEVEL=", DegreeLevel),
              DistanceED = paste0("DistanceED=", DistanceEd))

  #just this part
  write.table(x = partB, sep = ",",
              file = paste0(path, "Completions_PartB_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  #append to the upload doc
  write.table(x = partB, sep = ",",
              file = paste0(path, "Completions_PartsAll_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
}
