#' Shortcut function to do all steps to produce a report
#'
#' @param df A dataframe set up according to the readme
#' @param extracips A dataframe set up according to the readme (optional)
#' @param part A string with what part of the reprt you want to produce: 'all', 'A', etc.
#'
#' @return A txt file at the path of your choice
#' @export
#'
produce_comp_report <- function(df, extracips = NULL, part = 'all') {

  students <- prep_com_data_frame(df = df)
  extracips <- prep_com_data_frame(df = extracips)

  if(toupper(part) == 'ALL'){
    make_com_part_A(df = students, extracips = extracips, output = 'full')
    make_com_part_B(df = students, extracips = extracips, output = 'full')
    make_com_part_C(df = students, output = 'full')
    make_com_part_D(df = students, extracips = extracips, output = 'full')
  }
  if(part != 'all') {
    do.call(paste0("make_com_part_", toupper(part)), list(df, extracips))
  }
}

