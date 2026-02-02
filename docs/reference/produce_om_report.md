# Shortcut function with all steps to provide an Outcome Measures report

Shortcut function with all steps to provide an Outcome Measures report

## Usage

``` r
produce_om_report(df, part = "ALL", format = "uploadable")
```

## Arguments

- df:

  A dataframe set up according to the readme

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

## Examples

``` r
#entire report
produce_om_report(om_students)
#> Uploadable results available at C:/Users/alanski/AppData/Local/Temp/RtmpuCaqv0/OutcomeMeasures_AllParts_2026-02-02.txt

#one part with csv output instead of key-value
produce_om_report(om_students, part = 'A', format = 'readable')
#> Readable results available at  C:/Users/alanski/AppData/Local/Temp/RtmpuCaqv0/OutcomeMeasures_PartA_Readable_2026-02-02.csv
```
