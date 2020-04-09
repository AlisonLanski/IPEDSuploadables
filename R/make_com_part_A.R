#' Make Completions Part A
#'
#' @param df
#' @param extracips
#'
#' @return
#' @export
#'
#' @examples
make_com_part_A <- function(df, extracips = NULL) {

  #produce the uploadable format
  partA <- df %>%
    #aggregate the full data
    group_by(Unitid, MajorNumber, MajorCip, DegreeLevel, RaceEthnicity, Sex) %>%
    summarize(Count = n()) %>%
    ungroup()

  #prep the extra cips
  if (!is.null(extracips)) {
    #add extra cips
    partA <- extracips %>%
      select(Unitid, MajorNumber, MajorCip, DegreeLevel, RaceEthnicity, Sex, Count) %>%
      bind_rows(partA)
  }

  #carry on
  partA <- partA %>%
    #sort for easy viewing
    arrange(MajorNumber, MajorCip, DegreeLevel, RaceEthnicity, Sex) %>%
    #format for upload
    transmute(UNITID = paste0("UNITID=", Unitid),
              SURVSECT = "SURVSECT=COM",
              PART = "PART=A",
              MAJORNUM = paste0("MAJORNUM=", MajorNumber),
              CIPCODE = paste0("CIPCODE=", MajorCip),
              AWLEVEL = paste0("AWLEVEL=", DegreeLevel),
              RACE = paste0("RACE=", RaceEthnicity),
              SEX = paste0("SEX=", Sex),
              COUNT = paste0("COUNT=", Count))

  #just this part
  write.table(x = partA, sep = ",",
              file = paste0(path, "Completions_PartA_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)
}
