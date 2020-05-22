
#' Write the prepared data to a txt file in IPEDS format
#'
#' @param df a dataframe (prepared via the 'make' scripts)
#' @param component a string (which IPEDS survey)
#' @param part a string (which upload part of the survey)
#' @param path a string (where the txt file should be saved)
#' @param output a string (wanting to write just this part, the entire report, or both)
#' @param append a logical (if the txt file should append this part (T) or overwrite the entire file(F))
#'
#' @return a txt file (at the path location)
#' @importFrom utils write.table

write_report <- function(df, component, part, path, output, append) {

  if(tolower(output) == 'part' | output == 'both') {
    write.table(x = df, sep = ",",
                file = paste0(path, component, "_", part, "_", Sys.Date(), ".txt"),
                quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
  if(tolower(output) == 'full' | output == 'both'){
    write.table(x = df, sep = ",",
                file = paste0(path, component, "_AllParts_", Sys.Date(), ".txt"),
                quote = FALSE, row.names = FALSE, col.names = FALSE, append = append)
  }
}


