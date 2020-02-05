#####
## A function for making readable files in order to double check
## uploaded IPEDS data.
#####

library(stringr)

make_pretty_files <- function(df = partA) {
  
  dt <- data.frame()
  for(i in 1:nrow(df)) {
    for(j in 1:ncol(df)) {
      dt[i,j] <- str_replace_all(df[i,j], "^[:upper:]+[=]*", "")
    }
  }
  names(dt) <- names(df)
  
  return(dt)
}

# df <- make_pretty_files(partA)


