#####
## A function for making readable files in order to double check
## uploaded IPEDS data.
#####

make_pretty_files <- function(df = partA) {
  
  df %>%
    purrr::map_df(~stringr::str_replace_all(., "^[:upper:]+[=]*", ""))
}

## Example
df <- make_pretty_files(partA)


