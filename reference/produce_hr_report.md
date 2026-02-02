# Shortcut function with all steps to provide a Human Resources report

Shortcut function with all steps to provide a Human Resources report

## Usage

``` r
produce_hr_report(df, part = "all", format = "uploadable")
```

## Arguments

- df:

  a dataframe set up according to the readme

- part:

  a string with what part of the report you want to produce "all", "A1",
  etc.

- format:

  A string (`"uploadable"` will produce a properly formatted upload
  file. `"readable"` will produce a csv of the upload file (only works
  for one part at a time). `"both"` will provide both options, but only
  works with one part at a time.

## Value

A txt or csv file at the path of your choice

## Examples

``` r
# \donttest{
#entire report
produce_hr_report(hr_staff)
#> Uploadable results available at /private/var/folders/yz/zr09txvs5dn18vt4cn21kzl40000gn/T/RtmpFEfW0A/HumanResources_AllParts_2026-02-02.txt

#subsection with csv output instead of key-value txt
produce_hr_report(hr_staff, part = "A1", format = "readable")
#> Readable results available at  /private/var/folders/yz/zr09txvs5dn18vt4cn21kzl40000gn/T/RtmpFEfW0A/HumanResources_PartA1_Readable_2026-02-02.csv
# }
```
