# Shortcut function to do all steps to produce a report

Shortcut function to do all steps to produce a report

## Usage

``` r
produce_adm_report(df, ptype = 7, part = "ALL", format = "uploadable")
```

## Arguments

- df:

  A dataframe set up according to the readme

- ptype:

  (Optional) An integer \[1-9\] indicating which calculation method to
  use for test score percentiles. The default value within R and here
  is 7. To see details, run
  [`?quantile`](https://rdrr.io/r/stats/quantile.html) and scroll down
  to "Type".

- part:

  A string with what part of the report you want to produce: 'all', 'A',
  etc.

- format:

  A string (`"uploadable"` will produce a properly formatted upload
  file. `"readable"` will produce a csv of the upload file (only works
  for one part at a time). `"both"` will provide both options, but only
  works with one part at a time.

## Value

A txt or csv file at the path of your choice
