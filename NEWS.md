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
