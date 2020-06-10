#' Make Completions Part C
#'
#' @param df
#'
#' @importFrom rlang .data
#' @importFrom dplyr select group_by summarize ungroup arrange transmute
#'
#' @return
#' @export
#'
#' @examples
make_com_part_C <- function(df) {

  partC <- df %>%
    dplyr::select(.data$Unitid, .data$StudentId, .data$RaceEthnicity, .data$Sex) %>%
    #deduplicate
    unique() %>%
    #aggregate and count
    dplyr::group_by(.data$Unitid, .data$RaceEthnicity, .data$Sex) %>%
    dplyr::summarize(Count = n()) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$RaceEthnicity, .data$Sex) %>%
    #format for upload
    dplyr::transmute(UNITID = paste0("UNITID=", .data$Unitid),
                     SURVSECT = "SURVSECT=COM",
                     PART = "PART=C",
                     RACE = paste0("RACE=", .data$RaceEthnicity),
                     SEX = paste0("SEX=", .data$Sex),
                     COUNT = paste0("COUNT=", .data$Count)
                     )

  #just this part
  write.table(x = partC, sep = ",",
              file = paste0(path, "Completions_PartC_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  #append to the upload doc
  write.table(x = partC, sep = ",",
              file = paste0(path, "Completions_PartsAll_", Sys.Date(), ".txt"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
}
