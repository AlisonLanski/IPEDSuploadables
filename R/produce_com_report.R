#' Shortcut function with all steps to provide a Completions report
#'
#' @param df A dataframe set up according to the readme
#' @param extracips A dataframe set up according to the readme (optional)
#' @param part A string with what part of the report you want to produce: 'all',
#'   'A', etc.
#' @param format A string (\code{"uploadable"} will produce a properly formatted
#'   upload file. \code{"readable"} will produce a csv of the upload file (only
#'   works for one part at a time). \code{"both"} will provide both options, but
#'   only works with one part at a time.
#' @param ugender `r lifecycle::badge("deprecated")` A boolean: TRUE means you are collecting and able to report
#'   "another gender" for undergraduate completers, even if you have no (or few)
#'   such students. Set as FALSE if necessary. **Starting in 2025-2026, this argument will be ignored by later
#'   code.**
#' @param ggender `r lifecycle::badge("deprecated")` A boolean: TRUE means you are collecting and able to report
#'   "another gender" for graduate completers, even if you have no (or few) such
#'   students. Set as FALSE if necessary. **Starting in 2025-2026, this argument will be ignored by later
#'   code.**
#'
#'
#' @return A txt or csv file at the path of your choice
#' @export
#'
#'@examples
#'\dontshow{
#' #set temp directory for this example (not necessary for users)
#'.old_wd <- setwd(tempdir())
#'}
#'\donttest{
#'#entire report
#'produce_com_report(com_students, com_cips)
#'
#'#one part as csv instead of key-value
#'produce_com_report(com_students, com_cips, part = "A", format = "readable")
#'}
#'
#'\dontshow{
#' #reset directory for this example (not necessary for users)
#'setwd(.old_wd)
#'}

produce_com_report <- function(df, extracips = NULL, part = "ALL", format = "uploadable",
                               ugender = lifecycle::deprecated(), ggender = lifecycle::deprecated()) {

  if (lifecycle::is_present(ugender)) {
    lifecycle::deprecate_warn(
      when = "2.11.0",
      what = "produce_com_report(ugender)",
      details = "Detailed gender reporting is no longer used for this IPEDS survey. Argument may be removed in future versions."
    )
  }

  if (lifecycle::is_present(ggender)) {
    lifecycle::deprecate_warn(
      when = "2.11.0",
      what = "produce_com_report(ggender)",
      details = "Detailed gender reporting is no longer used for this IPEDS survey. Argument may be removed in future versions."
    )
  }

  stopifnot(toupper(part) %in% c("A", "B", "C", "D", "E", "ALL"),
            toupper(format) %in% c("UPLOADABLE", "READABLE", "BOTH"))

  #setup
  students <- prep_com_data_frame(df = df)

  if (!is.null(extracips)) {
    extracips <- prep_com_data_frame(df = extracips)
  }

  output_path <- set_report_path()
  survey = 'Completions'


  #uploadable means spit out a txt file
  if(toupper(part) == 'ALL') {

    write_report(
      make_com_part_A(df = students, extracips = extracips),
      make_com_part_B(df = students, extracips = extracips),
      make_com_part_C(df = students),
      make_com_part_D(df = students, extracips = extracips),
      make_com_part_E(df = students),
      survey = survey,
      part = 'AllParts',
      output_path = output_path
    )

  }

  if(toupper(part) %in% c("A", "B", "C", "D", "E")) {

    if(toupper(format) %in% c("UPLOADABLE", "BOTH")){

      if(toupper(part) == 'C'){
        write_report(
          make_com_part_C(students),
          survey = survey,
          part = paste0("Part", toupper(part)),
          output_path = output_path
          )
      } else if(toupper(part) == 'E'){
         write_report(
          make_com_part_E(students),
          survey = survey,
          part = paste0("Part", toupper(part)),
          output_path = output_path
           )
        } else {
          write_report(
            do.call(paste0("make_com_part_", toupper(part)), list(students, extracips)),
            survey = survey,
            part = paste0("Part", toupper(part)),
            output_path = output_path
            )
          }

    }

    if(toupper(format) %in% c("BOTH", "READABLE")){
      if(toupper(part) == 'C'){
        write_report_csv(
          do.call(paste0("make_com_part_", toupper(part)), list(students)),
          survey = survey,
          part = paste0("Part", toupper(part)),
          output_path = output_path
          )
      } else if(toupper(part) == 'E'){
        write_report_csv(
          do.call(paste0("make_com_part_", toupper(part)), list(students)),
          survey = survey,
          part = paste0("Part", toupper(part)),
          output_path = output_path
        )
      } else {
        write_report_csv(
          do.call(paste0("make_com_part_", toupper(part)), list(students, extracips)),
          survey = survey,
          part = paste0("Part", toupper(part)),
          output_path = output_path
          )
      }
    }

  }

}

