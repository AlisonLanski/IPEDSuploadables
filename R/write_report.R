#' Write the prepared data to a txt file in IPEDS format
#'
#' @param df a dataframe (prepared via the 'make' scripts)
#' @param component a string (which IPEDS survey)
#' @param part a string (which upload part of the survey)
#' @param output a string (wanting to write just this part, the entire report, or both)
#' @param append a logical (if the txt file should append this part (T) or overwrite the entire file(F))
#' @param format A string (\code{"uploadable"}, \code{"readable"}, or \code{"both"})
#'
#' @return a txt file (at the path location)
#' @importFrom utils write.table
#' @importFrom purrr map_df
#' @importFrom stringr str_replace_all

write_report <- function(df, component, part, output, append = FALSE, format = "uploadable") {

  while (!exists(x = "output_path", envir = globalenv())) {
    set_report_path()
  }

  if (toupper(output) == "PART" | toupper(output) == "BOTH") {
    if (toupper(format) == "UPLOADABLE" | toupper(format) == "BOTH") {
      write.table(x = df, sep = ",",
                  file = paste0(output_path, component, "_", part, "_", Sys.Date(), ".txt"),
                  quote = FALSE, row.names = FALSE, col.names = FALSE)
    } else if (toupper(format) == "READABLE" | toupper(format) == "BOTH") {
      df %>%
        purrr::map_df(~stringr::str_replace_all(., "^[:upper:]+[=]*", "")) %>%
        write.table(sep = ",",
                    file = paste0(output_path, "Readable_", component, "_", part, "_", Sys.Date(), ".csv"),
                    quote = FALSE, row.names = FALSE, col.names = TRUE)
    }
  }

  if (toupper(output) == "FULL" | toupper(output) == "BOTH") {
    if (toupper(format) == "UPLOADABLE" | toupper(format) == "BOTH") {
      if (grepl(part, pattern = ('A$|A1'))) {
        append <- FALSE
      } else {
        append <- TRUE
      }
      write.table(x = df, sep = ",",
                  file = paste0(output_path, component, "_AllParts_", Sys.Date(), ".txt"),
                  quote = FALSE, row.names = FALSE, col.names = FALSE, append = append)
    } else if (toupper(format) == "READABLE" | toupper(format) == "BOTH") {
      df %>%
        purrr::map_df(~stringr::str_replace_all(., "^[:upper:]+[=]*", "")) %>%
        write.table(sep = ",",
                    file = paste0(output_path, "Readable_", component, "_AllParts_", Sys.Date(), ".csv"),
                    quote = FALSE, row.names = FALSE, col.names = TRUE, append = append)
    }
  }

  print(paste0("Results available at ", output_path, component, ". To change the path, please run set_report_path()'"))
}


