#' Make Completions Part C
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
make_com_part_C <- function(df) {

  partC <- df %>%
    select(Unitid, StudentId, RaceEthnicity, Sex) %>%
    #deduplicate
    unique() %>%
    #aggregate and count
    group_by(Unitid, RaceEthnicity, Sex) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    #sort for easy viewing
    arrange(RaceEthnicity, Sex) %>%
    #format for upload
    transmute(UNITID = paste0("UNITID=", Unitid),
              SURVSECT = "SURVSECT=COM",
              PART = "PART=C",
              RACE = paste0("RACE=", RaceEthnicity),
              SEX = paste0("SEX=", Sex),
              COUNT = paste0("COUNT=", Count))

  #just this part
  write.table(x = partC, sep = ",",
              file = paste0(path, "Completions_PartC_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  #append to the upload doc
  write.table(x = partC, sep = ",",
              file = paste0(path, "Completions_PartsAll_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
}
