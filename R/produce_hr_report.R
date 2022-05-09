#' Shortcut function with all steps to provide a Human Resources report
#'
#' @param df a dataframe set up according to the readme
#' @param part a string with what part of the report you want to produce "all", "A1", etc.
#' @param format A string (\code{"uploadable"} will produce a properly formatted
#'   upload file. \code{"readable"} will produce a csv of the upload file (only
#'   works for one part at a time). \code{"both"} will provide both options, but
#'   only works with one part at a time.
#'
#' @return A txt or csv file at the path of your choice
#' @export
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' #entire report
#' produce_hr_report(hr_staff)
#' #subsection with csv output instead of key-value txt
#' produce_hr_report(hr_staff, part = "A1", format = "readable")
#' \dontshow{
#' setwd(.old_wd)
#' }

produce_hr_report <- function(df, part = "all", format = "uploadable") {

  stopifnot(toupper(part) %in% c("A1", "A2", "B1", "B2", "B3", "D1", "D2", "D3", "D4", "G1", "G2", "H1", "H2", "ALL"),
            toupper(format) %in% c("UPLOADABLE", "READABLE", "BOTH"))

  survey <- "HumanResources"
  output_path <- set_report_path()
  df <- prep_hr_data_frame(df)

  if(toupper(part) == "ALL") {

    write_report(
      make_hr_part_A1(df),
      make_hr_part_A2(df),
      make_hr_part_B1(df),
      make_hr_part_B2(df),
      make_hr_part_B3(df),
      make_hr_part_D1(df),
      make_hr_part_D2(df),
      make_hr_part_D3(df),
      make_hr_part_D4(df),
      make_hr_part_G1(df),
      make_hr_part_G2(df),
      make_hr_part_H1(df),
      make_hr_part_H2(df),
      survey = survey,
      part = 'AllParts',
      output_path = output_path
    )

  }

  if(toupper(part) != "ALL" & toupper(format) %in% c("UPLOADABLE", "BOTH")) {
    write_report(
      do.call(paste0("make_hr_part_", toupper(part)), list(df)),
      survey = survey,
      part = paste0("Part", toupper(part)),
      output_path = output_path
    )
  }

  if(toupper(part) != "ALL" & toupper(format) %in% c("READABLE", "BOTH")) {
    write_report_csv(
      do.call(paste0("make_hr_part_", toupper(part)), list(df)),
      survey = survey,
      part = paste0("Part", toupper(part)),
      output_path = output_path
    )
  }
}
