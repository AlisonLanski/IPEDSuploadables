# IPEDSuploadables 2.10.0
* Updating incorrect column name in Outcome Measures vignette
* Added quality check for correct column name in OM data prep script
  
# IPEDSuploadables 2.9.1
* Changing data load method in vignettes to avoid error on old Mac OS checks
* Making function documentation a bit more uniform
* Adding some badges for fun

# IPEDSUploadables 2.9.0
* Addressed 2024-2025 reporting cycle updates from IPEDS (see items below)
* Graduation rates removed Gender Detail; info longer required in starting data (functions deprecated)
* 12 month enrollment now collects/reports on what kind of program a high school student is in (dual or other)
* Internal R updates to handle roxygen & dplyr changes
* Some documentation cleanup

# IPEDSUploadables 2.8.7
* Internal updates to address CRAN notes about web data and startup messages  

# IPEDSUploadables 2.8.6
* Documenting "Count" as a required column for the Completions extracips dataframe  
  This is a documentation update only; no changes were made to functions; this version was not pushed to CRAN

# IPEDSUploadables 2.8.5
* Addressed 2023-2024 reporting cycle updates from IPEDS (see items below)
* Gender Detail masking implemented when "Another Gender" has fewer than 5 reportable students
* 12 Month Enrollment has a new flag and section for Dual Enrollment (High School) students 
* Related documentation updates
* Syntax updates for compatibility with new versions of roxygen and tidyselect

# IPEDSuploadables 2.7.5

* Addressed Spring Collection updates from IPEDS (see items below)
* Fall Enrollment data now requires a GenderDetail column in the student data for this year's reporting
* Human Resources data now excludes New Hires that are not Current Employees from reporting; the functions will now throw a warning for anyone in violation.
* Changes union_all code to avoid an error with the next version of R


# IPEDSuploadables 2.6.5

* Addressed Winter Collection updates from IPEDS (see items below)
* Graduation Rates data now requires a GenderDetail column for this year's reporting
* Admissions example in the How-To vignette includes columns for new application-related questions and new another-gender counts

# IPEDSuploadables 2.5.5

* Fixed Completions bug for cip codes in XX.0000 format
* Addressed Fall Collection updates from IPEDS (see items below)
* Completions data now requires a GenderDetail column for this year's reporting
* 12 month enrollment now requires a GenderDetail column for this year's reporting
* 12 month enrollment now provides full/part time breakouts for Graduate students

# IPEDSuploadables 2.4.5

* Added a `NEWS.md` file to track changes to the package.
* Added internal tests and user-facing data quality messages
* Documentation tweaks for clarity and CRAN preparation
* Adding handling to completions for cip codes in XXYYYY format

# IPEDSuploadables 2.3.5

* See GitHub pull requests/releases for details of previous updates
