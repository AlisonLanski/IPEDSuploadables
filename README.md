
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IPEDSuploadables

<!-- badges: start -->

<!-- badges: end -->

IPEDSuploadables produces text files in key-value format which meet the
IPEDS requirements for upload on their website. By using this package,
institutions can avoid the time and risk of manually typing numbers into
web forms across multiple screen. Package documentation (see the
[website](https://alisonlanski.github.io/IPEDSuploadables/)) describes
how an institution’s data must be formatted to make use of these
scripts.

## Supported Reports

*As reports are added, this list will be updated.  
You are welcome to fork this repo and work towards new reports or new
institution-types for existing reports*

### Surveys that work for all institutions:

  - Completions
  - Outcome Measures

### Surveys that only work for some institutions:

  - Graduation Rates: 4-year institutions
  - 200% Graduation Rates: 4-year institutions
  - 12-month Enrollment: 4-year degree granting institutions
  - Fall Enrollment: 4-year degree granting institutions
  - Human Resources: degree granting institutions with more than 15 FTE

## Website

View the pkgdown version of this site and documentation at
<https://alisonlanski.github.io/IPEDSuploadables/>

## Installation

You can install the development version from
[GitHub](https://github.com/AlisonLanski/IPEDSuploadables) with:

``` r
# install.packages("devtools")
devtools::install_github("AlisonLanski/IPEDSuploadables")

#or use

# install.packages("githubinstall")
githubinstall::install_github("AlisonLanski/IPEDSuploadables")
```

To access vignettes locally (instead of through the website) add an
argument:

``` r
devtools::install_github("AlisonLanski/IpedsUpladables", build_vignettes = TRUE)
```
