#' Shortcut function with all steps to provide a Fall Enrollment report
#'
#' @param students A dataframe set up according to the readme with student data
#' @param retention A dataframe set up according to the readme with retention
#'   data
#' @param part A string with what part of the report you want to produce: 'all',
#'   'A', etc.
#' @param include_optional A boolean flag for whether optional parts should be
#'   included
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
#' @examples
#' \dontshow{
#' #set temp directory for this example (not necessary for users)
#' .old_wd <- setwd(tempdir())
#' }
#'
#' \donttest{
#' #entire report
#' produce_ef1_report(ef1_students, ef1_retention)
#'
#' #entire report with optional sections
#' produce_ef1_report(ef1_students, ef1_retention, include_optional = TRUE)
#'
#' #one part as csv instead of key-value
#' produce_ef1_report(ef1_students, part = 'D', format = 'readable')
#' }
#'
#' \dontshow{
#' #reset directory for this example (not necessary for users)
#' setwd(.old_wd)
#' }

produce_ef1_report <- function(students, retention, part = "ALL", include_optional = FALSE,
                               format = "uploadable", ugender = lifecycle::deprecated(), ggender = lifecycle::deprecated()) {

  stopifnot(toupper(part) %in% c("A", "B", "C", "D", "E", "F", "G", "H", "ALL"),
            toupper(format) %in% c("UPLOADABLE", "READABLE", "BOTH"))

  if (lifecycle::is_present(ugender)) {
    lifecycle::deprecate_warn(
      when = "2.11.0",
      what = "produce_ef1_report(ugender)",
      details = "Detailed gender reporting is no longer used for this IPEDS survey. Argument may be removed in future versions."
    )
  }

  if (lifecycle::is_present(ggender)) {
    lifecycle::deprecate_warn(
      when = "2.11.0",
      what = "produce_ef1_report(ggender)",
      details = "Detailed gender reporting is no longer used for this IPEDS survey. Argument may be removed in future versions."
    )
  }

  #setup
  students <- prep_ef1_data_frame(students)

  survey <- 'FallEnrollment'
  output_path <- set_report_path()

  cip_year <- ((as.numeric(substr(Sys.Date(), 6, 7)) >= 8) + as.numeric(substr(Sys.Date(), 1, 4))) %% 2 == 1


  if (toupper(part) == "ALL") {
    partA <- make_ef1_part_A(df = students, cips = cip_year)
    partG <- make_ef1_part_G(df = students)

    #parts B/C are optional in some years;
    #this allows them to be included by rule (based on time) or by election
    #we still need to produce a DF so the call to write_report doesn't fail
    #but write_report itself has been updated to remove empty DFs, so the final file is ok
    if (cip_year == FALSE | include_optional == TRUE) {
      partB <- make_ef1_part_B(df = students)
    } else {partB <- data.frame()}

    if (cip_year == TRUE | include_optional == TRUE) {
      partC <- make_ef1_part_C(df = students)
    } else {partC <- data.frame()}

    partD <- make_ef1_part_D(df = students)
    partE <- make_ef1_part_E(df = retention)
    partF <- make_ef1_part_F(df = students)
    partH <- make_ef1_part_H(df = students)

    if(toupper(format) == 'UPLOADABLE'){
      write_report(
        partA,
        partG,
        partB,
        partC,
        partD,
        partE,
        partF,
        partH,
        survey = survey,
        part = 'AllParts',
        output_path = output_path)
    } else {

      message("Uploadable is the only supported format type when requesting all parts")
    }

  } else if(toupper(part) %in% c('A','B', 'C', 'D', 'E', 'F', 'G', 'H')){

    if(toupper(part) == "A") {
      partX <- do.call(paste0("make_ef1_part_", toupper(part)), list(students, cip_year))
    }

    if (toupper(part) %in% c("G", "B", "C", "D", "F")) {
      #don't have to do a special cipyear etc thing, because if they're asking for the part, we'll make it
      partX <- do.call(paste0("make_ef1_part_", toupper(part)), list(students))
    }

    if (toupper(part) == "H") {
      partX <- do.call(paste0("make_ef1_part_", toupper(part)), list(students))
    }


    if (toupper(part) == "E") {
      partX <- do.call(paste0("make_ef1_part_", toupper(part)), list(retention))
    }

    if(toupper(format) %in% c('UPLOADABLE', 'BOTH')){
      write_report(
        partX,
        survey = survey,
        part = paste0("Part", toupper(part)),
        output_path = output_path
      )
    }
    if(toupper(format) %in% c('READABLE', 'BOTH')){
      write_report_csv(
        partX,
        survey = survey,
        part = paste0("Part", toupper(part)),
        output_path = output_path
      )
    }
  }

}

