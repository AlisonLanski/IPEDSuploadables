#' Shortcut function with all steps to provide a Graduation Rates report
#'
#' @param df a dataframe set up according to the readme
#' @param part a string with what part of the report you want to produce "all",
#'   "A1", etc.
#' @param format A string (\code{"uploadable"} will produce a properly formatted
#'   upload file. \code{"readable"} will produce a csv of the upload file (only
#'   works for one part at a time). \code{"both"} will provide both options, but
#' only works with one part at a time.
#' @return A txt or csv file at the path of your choice
#' @export
#' @examples
#' \dontshow{
#' #set temp directory for this example (not necessary for users)
#' .old_wd <- setwd(tempdir())
#' }
#' \donttest{
#' #entire report
#' produce_gr_report(gr_students)
#'
#' #one part in csv format instead of key-value
#' produce_gr_report(gr_students, part = "B", format = "readable")
#' }
#' \dontshow{
#' #reset directory for this example (not necessary for users)
#' setwd(.old_wd)
#' }

produce_gr_report <- function(df, part = "ALL", format = "uploadable") {

  stopifnot(toupper(part) %in% c("B", "C", "ALL"),
            toupper(format) %in% c("UPLOADABLE", "READABLE", "BOTH"))

  survey <- 'GradRates'
  output_path <- set_report_path()

  if(toupper(part) == 'ALL') {

    write_report(
      make_gr_part_B(df),
      make_gr_part_C(df),
      survey = survey,
      part = 'AllParts',
      output_path = output_path
    )

  } else if(toupper(part) %in% c("B", "C")) {

    if(toupper(format) %in% c("UPLOADABLE", "BOTH")){
      write_report(
        do.call(paste0("make_gr_part_", toupper(part)), list(df)),
        survey = survey,
        part = paste0("Part", toupper(part)),
        output_path = output_path
      )
    }

    if(toupper(format) %in% c("BOTH", "READABLE")){
      write_report_csv(
        do.call(paste0("make_gr_part_", toupper(part)), list(df)),
        survey = survey,
        part = paste0("Part", toupper(part)),
        output_path = output_path
      )
    }
  }
}
