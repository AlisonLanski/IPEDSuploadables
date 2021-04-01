#' Shortcut function to autoformat any file that's already aggregated.
#' Columns must already be in correct name format.
#'
#' @param df A dataframe
#'
#' @return A txt file at the path of your choice
#' @export
#'
create_autoupload_format_from_aggregate <- function(df) {

      for (j in 1:nrow(df)) {
          df %>% stringr::str_glue_data("{colnames(.)}={df[j]},") #%>% ## Need to figure out how to glue COLUMN_NAME=VALUE for every row
          # write.table(x = df, sep = ",",
          #             file = paste0(output_path, component, "_", part, "_", Sys.Date(), ".txt"),
          #             quote = FALSE, row.names = FALSE, col.names = FALSE)
      }
}

