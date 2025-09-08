
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IPEDSuploadables

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/IPEDSuploadables)](https://CRAN.R-project.org/package=IPEDSuploadables)
[![R-CMD-check](https://github.com/AlisonLanski/IPEDSuploadables/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AlisonLanski/IPEDSuploadables/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/AlisonLanski/IPEDSuploadables/graph/badge.svg)](https://app.codecov.io/gh/AlisonLanski/IPEDSuploadables)
[![Downloads](https://cranlogs.r-pkg.org/badges/IPEDSuploadables)](https://CRAN.R-project.org/package=IPEDSuploadables)
<!-- badges: end -->

IPEDSuploadables produces text files in key-value format which meet the
IPEDS requirements for upload on their website. By using this package,
institutions can avoid the time and risk of manually typing numbers into
web forms across multiple screen. Package documentation (see the
[website](https://alisonlanski.github.io/IPEDSuploadables/)) describes
how an institution’s data must be formatted to make use of these
scripts.

### The package is currently serving the 2025-2026 IPEDS reporting cycle

## If you are new to this process or are looking for updates

- Learn the package basics with the article [How to use this package to
  support your IPEDS
  reporting](https://alisonlanski.github.io/IPEDSuploadables/articles/howto_overall_ipedsuploadables.html)
- Check the package [news
  page](https://alisonlanski.github.io/IPEDSuploadables/news/index.html)
  for recent updates in this release

## Report Coverage

*As reports are added, this list will be updated.  
You are welcome to fork this repo and work towards new reports or new
institution-types for existing reports*

### Supported surveys that work for all institutions:

- Completions
- Outcome Measures

### Supported surveys that only work for some institutions:

- Graduation Rates: 4-year institutions
- 200% Graduation Rates: 4-year institutions
- 12-month Enrollment: 4-year degree granting institutions
- Fall Enrollment: 4-year degree granting institutions
- Human Resources: degree granting institutions with more than 15 FTE

### Unsupported surveys with options

- Academic Libraries: This survey is no longer collected by IPEDS as of
  the 2025-2026 reporting cycle. For previous years, you can download an
  IPEDS-compatible file from the ACRL site if you complete that
  survey.  
- Admissions: Visit [How to produce other key-value
  uploads](https://alisonlanski.github.io/IPEDSuploadables/articles/howto_use_autoformat.html)
- Cost I: Visit [How to produce other key-value
  uploads](https://alisonlanski.github.io/IPEDSuploadables/articles/howto_use_autoformat.html)
- Cost II: Visit [How to produce other key-value
  uploads](https://alisonlanski.github.io/IPEDSuploadables/articles/howto_use_autoformat.html)
- Finance: Visit [How to produce other key-value
  uploads](https://alisonlanski.github.io/IPEDSuploadables/articles/howto_use_autoformat.html)
- Institutional Characteristics: Visit [How to produce other key-value
  uploads](https://alisonlanski.github.io/IPEDSuploadables/articles/howto_use_autoformat.html)
- Student Financial Aid: Visit [How to produce other key-value
  uploads](https://alisonlanski.github.io/IPEDSuploadables/articles/howto_use_autoformat.html)

## Websites

For code, issues, etc, visit [the GitHub
site](https://github.com/AlisonLanski/IPEDSuploadables/).  
To see full documentation, visit [the package
website](https://alisonlanski.github.io/IPEDSuploadables/).

## Installation

You can install the package in the standard way from CRAN

    install.packages("IPEDSuploadables")

To get newer releases or bug fixes before they are posted to the CRAN
version, you can install the development version from
[GitHub](https://github.com/AlisonLanski/IPEDSuploadables) with:

``` r
# install.packages("devtools")
devtools::install_github("AlisonLanski/IPEDSuploadables")

#or use

# install.packages("remotes")
remotes::install_github("AlisonLanski/IPEDSuploadables")
```

To access vignettes locally (instead of through the website) add an
argument:

``` r
devtools::install_github("AlisonLanski/IPEDSuploadables", build_vignettes = TRUE)
```
