#' Shortcut function to do all steps to produce a report
#'
#' @param df A dataframe set up according to the readme
#' @param part A string with what part of the report you want to produce: 'all', 'A', etc.
#' @param ptype (Optional) An integer [1-9] indicating which calculation method to use for test score percentiles. The default value within R and here is 7. To see details, run \code{?quantile} and scroll down to "Type".
#'
#' @param format A string (\code{"uploadable"} will produce a properly formatted
#'   upload file. \code{"readable"} will produce a csv of the upload file (only
#'   works for one part at a time). \code{"both"} will provide both options, but
#'   only works with one part at a time.
#'
#' @return A txt or csv file at the path of your choice
#' @export
#'

produce_adm_report <- function(df, ptype = 7, part = "ALL", format = "uploadable") {

  stopifnot(toupper(part) %in% c("B", "F", "D", "G", "C", "H", "ALL"),
            toupper(format) %in% c("UPLOADABLE", "READABLE", "BOTH"))

  #setup
  students <- prep_adm_data_frame(df = df)
  output_path <- set_report_path()
  survey = "Admissions"

  #uploadable means spit out a txt file

  if(toupper(part) == 'ALL') {

    write_report(
      make_adm_part_B(df = students),
      make_adm_part_F(df = students),
      make_adm_part_D(df = students),
      make_adm_part_G(df = students),
      make_adm_part_C(df = students, ptype = ptype),
      make_adm_part_H(df = students, ptype = ptype),
      survey = survey,
      part = 'AllParts',
      output_path = output_path
    )
  }

  if(toupper(part) %in% c("B", "F", "D", "G", "C", "H")) {

    if(toupper(format) %in% c("UPLOADABLE", "BOTH")){
      if(toupper(part) %in% c("C", "H")){
        write_report(
          do.call(paste0("make_adm_part_", toupper(part)), list(students, ptype)),
          survey = survey,
          part = paste0("Part", toupper(part)),
          output_path = output_path
        )
      } else {
        write_report(
          do.call(paste0("make_adm_part_", toupper(part)), list(students)),
          survey = survey,
          part = paste0("Part", toupper(part)),
          output_path = output_path
        )
      }
    }

    if(toupper(format) %in% c("BOTH", "READABLE")){
      if(toupper(part) %in% c("C", "H")){
        write_report_csv(
          do.call(paste0("make_adm_part_", toupper(part)), list(students, ptype)),
          survey = survey,
          part = paste0("Part", toupper(part)),
          output_path = output_path
        )
      } else{
        write_report_csv(
          do.call(paste0("make_adm_part_", toupper(part)), list(students)),
          survey = survey,
          part = paste0("Part", toupper(part)),
          output_path = output_path
        )
      }
    }
  }

}
