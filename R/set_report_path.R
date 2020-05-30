
#' Set the path for where the reports will be saved to.
#'
#' @return path
#' @importFrom svDialogs dlg_dir
#' @importFrom rstudioapi selectDirectory
#' @importFrom stringr str_detect
#'
#'

set_report_path <- function() {

  #set an output path:
  #path <- svDialogs::dlg_dir(default = getwd(), title = "Select the file output location")$res
  path <- rstudioapi::selectDirectory()

  #make sure the final / is in the path (before the filename)
  if(!stringr::str_detect(path, pattern = "/$")) {
    path <- paste0(path, "/")
  }

  # Return path object to environment
  assign(x = "output_path", value = path, envir = globalenv())
}
