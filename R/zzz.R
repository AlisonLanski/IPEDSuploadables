.onAttach <- function(libname, pkgname){
  packageStartupMessage("This version of IPEDSuploadables has been updated for \nthe 2023-2024 reporting cycle.\nIf you are in a later cycle or want to capture any bug fixes,\nplease update the package.")
}

.onLoad <- function(libname, pkgname) {
  invisible(suppressPackageStartupMessages(
    sapply(c("dplyr", "lubridate", "magrittr", "purrr",
             "rlang", "stringr", "tidyr", "utils", "svDialogs"),
           requireNamespace, quietly = TRUE)
  ))
}
