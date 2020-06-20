ipedsEnv <- NULL
.onLoad <- function(...) {
  ipedsEnv <<- new.env() # when package is loaded, create new environment to store needed variables
}
