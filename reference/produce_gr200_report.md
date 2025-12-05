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
#> Uploadable results available at /private/var/folders/p6/nlmq3k8146990kpkxl73mq340000gn/T/Rtmp71pO15/GR200_AllParts_2025-12-05.txt
```
