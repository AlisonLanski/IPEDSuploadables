# Shortcut function with all steps to provide a Grad Rates 200 report

Shortcut function with all steps to provide a Grad Rates 200 report

## Usage

``` r
produce_gr200_report(df, format = "uploadable")
```

## Arguments

- df:

  a dataframe set up according to the readme

- format:

  A string (`"uploadable"` will produce a properly formatted upload
  file. `"readable"` will produce a csv of the upload file (only works
  for one part at a time). `"both"` will provide both options, but only
  works with one part at a time.

## Value

A txt or csv file at the path of your choice

## Examples

``` r
#entire report
produce_gr200_report(gr200_students)
#> Uploadable results available at /private/var/folders/yz/zr09txvs5dn18vt4cn21kzl40000gn/T/RtmpFEfW0A/GR200_AllParts_2026-02-02.txt
```
