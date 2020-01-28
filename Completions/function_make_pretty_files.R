#####
## A function for making readable files in order to double check
## uploaded IPEDS data.
#####

library(stringr)

make_pretty_files <- function(df = partA) {
  
  dt <- list()
  for(i in 1:ncol(df)) {
    dt[[i]] <- str_replace_all(df[[i]], "[:upper:]+[=]*", "")
  }
}

df <- make_pretty_files(partA)

df[[1]] <- str_replace_all(partA[[1]], "[:upper:]+[=]+", "")

partA[[1]]
