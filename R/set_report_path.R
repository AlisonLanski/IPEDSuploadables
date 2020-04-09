#' Set the path for where the reports will be saved to.
#'
#' @return path
#' @importFrom svDialogs dlg_dir
#' @importFrom stringr str_detect
#'
#' @examples
set_report_path <- function() {

  #set an output path:
  path <- svDialogs::dlg_dir(default = getwd(), title = "Select the file output location")$res

  #make sure the final / is in the path (before the filename)
  if(!stringr::str_detect(path, pattern = "/$")) {
    path <- paste0(path, "/")
  }

  return(path)
}
