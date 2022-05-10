#' Shortcut function with all steps to provide an Outcome Measures report
#'
#' @param df A dataframe set up according to the readme
#' @param part A string with what part of the report you want to produce: 'all',
#'   'A', etc.
#' @param format A string (\code{"uploadable"} will produce a properly formatted
#'   upload file. \code{"readable"} will produce a csv of the upload file (only
#'   works for one part at a time). \code{"both"} will provide both options, but
#'   only works with one part at a time.
#'
#' @return A txt or csv file at the path of your choice
#' @export
#' @examples
#' \dontshow{
#' #set temp directory for this example (not necessary for users)
#' .old_wd <- setwd(tempdir())
#' }
#'
#' #entire report
#' produce_om_report(om_students)
#'
#' #one part with csv output instead of key-value
#' produce_om_report(om_students, part = 'A', format = 'readable')
#'
#' \dontshow{
#' #reset directory for this example (not necessary for users)
#' setwd(.old_wd)
#' }

produce_om_report <- function(df, part = "ALL", format = "uploadable") {

  stopifnot(toupper(part) %in% c("A", "B", "C", "D", "ALL"),
            toupper(format) %in% c("UPLOADABLE", "READABLE", "BOTH"))

  students <- prep_om_data_frame(df = df)
  output_path <- set_report_path()
  survey = 'OutcomeMeasures'

  #uploadable means spit out a txt file
  if(toupper(part) == 'ALL') {

      write_report(
        make_om_part_A(df = students),
        make_om_part_B(df = students),
        make_om_part_C(df = students),
        make_om_part_D(df = students),
        survey = survey,
        part = 'AllParts',
        output_path = output_path
        )

    } else if(toupper(part) %in% c("A", "B", "C", "D")) {

        if(toupper(format) %in% c("UPLOADABLE", "BOTH")){
          write_report(
            do.call(paste0("make_om_part_", toupper(part)), list(students)),
            survey = survey,
            part = paste0("Part", toupper(part)),
            output_path = output_path
          )
        }

        if(toupper(format) %in% c("BOTH", "READABLE")){
          write_report_csv(
            do.call(paste0("make_om_part_", toupper(part)), list(students)),
            survey = 'OutcomeMeasures',
            part = paste0("Part", toupper(part)),
            output_path = output_path
            )
        }

      }

}

