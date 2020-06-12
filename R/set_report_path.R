#' Set the path for where the reports will be saved to.
#'
#' @return path
#' @importFrom svDialogs dlg_dir
#' @importFrom stringr str_detect
#'
set_report_path <- function() {

  #set an output path:
  path <- svDialogs::dlg_dir(default = getwd(), title = "Select the file output location")$res

  #make sure the final / is in the path (before the filename)
  if(!stringr::str_detect(path, pattern = "/$")) {
    path <- paste0(path, "/")
  }

<<<<<<< HEAD
  # Return path object to environment
  # Better not to send it to global, but figuring out a real environment is hard
  assign(x = "output_path", value = path, envir = globalenv())
>>>>>>> 09472509b547e337360a9f5859cf07773f3c6afa
}
