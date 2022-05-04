#' Output files from the make functions using the package dummy data (for testing)
#' @description Contains a list of output files from each "make" script which can be called by testthat tests to ensure refactoring doesn't change output inappropriately
#' @format A list of dataframes: one dataframe per make script
#' @details General list-creation code is commented out in the test-part-outputs.R script; uncomment and run/add as needed if a df needs a legitimate update (e.g. IPEDS rules change, dummy data includes new use case)
#' @examples \dontrun{part_outputs$com_partD}

"part_outputs"
