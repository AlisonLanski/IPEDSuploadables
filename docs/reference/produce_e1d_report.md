# Shortcut function with all steps to provide a 12 Month Enrollment report

Shortcut function with all steps to provide a 12 Month Enrollment report

## Usage

``` r
produce_e1d_report(
  df,
  hrs,
  part = "ALL",
  format = "uploadable",
  ugender = lifecycle::deprecated(),
  ggender = lifecycle::deprecated()
)
```

## Arguments

- df:

  A dataframe set up according to the readme for students

- hrs:

  A dataframe set up according to the readme for instructional activity

- part:

  A string with what part of the report you want to produce: 'all', 'A',
  etc.

- format:

  A string (`"uploadable"` will produce a properly formatted upload
  file. `"readable"` will produce a csv of the upload file (only works
  for one part at a time). `"both"` will provide both options, but only
  works with one part at a time.

- ugender:

  \`r lifecycle::badge("deprecated")\` A boolean: TRUE means you are
  collecting and able to report "another gender" for undergraduate
  completers, even if you have no (or few) such students. Set as FALSE
  if necessary. \*\*Starting in 2025-2026, this argument will be ignored
  by later code.\*\*

- ggender:

  \`r lifecycle::badge("deprecated")\` A boolean: TRUE means you are
  collecting and able to report "another gender" for graduate
  completers, even if you have no (or few) such students. Set as FALSE
  if necessary. \*\*Starting in 2025-2026, this argument will be ignored
  by later code.\*\*

## Value

A txt or csv file at the path of your choice

## Examples

``` r
# \donttest{
#entire report
produce_e1d_report(e1d_students, e1d_instr)
#> Uploadable results available at C:/Users/alanski/AppData/Local/Temp/RtmpeUwoC9/12MonthEnrollment_AllParts_2026-02-02.txt

#one part, as csv instead of key-value file
produce_e1d_report(e1d_students, part = "A", format = "readable")
#> Readable results available at  C:/Users/alanski/AppData/Local/Temp/RtmpeUwoC9/12MonthEnrollment_PartA_Readable_2026-02-02.csv
# }
```
