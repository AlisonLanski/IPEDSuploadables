
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

## Report Coverage

*As reports are added, this list will be updated.  
You are welcome to fork this repo and work towards new reports or new
institution-types for existing reports*

### Supported surveys that work for all institutions:

-   Completions *Updated for current cycle*
-   Outcome Measures

### Supported surveys that only work for some institutions:

-   Graduation Rates: 4-year institutions
-   200% Graduation Rates: 4-year institutions
-   12-month Enrollment: 4-year degree granting institutions *Updated
    for current cycle*
-   Fall Enrollment: 4-year degree granting institutions
-   Human Resources: degree granting institutions with more than 15 FTE

### Unsupported surveys with options

-   Academic Libraries: If you complete the ACRL survey, you can
    download an IPEDS-compatible file from the ACRL site.
-   Admissions: Visit [How to produce other key-value
    uploads](https://alisonlanski.github.io/IPEDSuploadables/articles/howto_use_autoformat.html)
-   Finance: Visit [How to produce other key-value
    uploads](https://alisonlanski.github.io/IPEDSuploadables/articles/howto_use_autoformat.html)
-   Institutional Characteristics: Visit [How to produce other key-value
    uploads](https://alisonlanski.github.io/IPEDSuploadables/articles/howto_use_autoformat.html)
-   Student Financial Aid: Visit [How to produce other key-value
    uploads](https://alisonlanski.github.io/IPEDSuploadables/articles/howto_use_autoformat.html)

## Website

View the pkgdown version of this site and documentation at
<https://alisonlanski.github.io/IPEDSuploadables/>

View the codebase and submit issues for this package at
<https://github.com/AlisonLanski/IPEDSuploadables/>

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
