# Shortcut function with all steps to provide a Graduation Rates report

Shortcut function with all steps to provide a Graduation Rates report

## Usage

``` r
produce_gr_report(
  df,
  part = "ALL",
  format = "uploadable",
  ugender = lifecycle::deprecated()
)
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

- ugender:

  \`r lifecycle::badge("deprecated")\` A boolean: TRUE means you are
  collecting and able to report "another gender" for undergraduate
  students, even if you have no (or few) such students. Set as FALSE if
  necessary. \*\*Starting in 2024-2025, this argument will be ignored by
  later code.\*\*

## Value

A txt or csv file at the path of your choice

## Examples

``` r
# \donttest{
#entire report
produce_gr_report(gr_students)
#> Uploadable results available at /private/var/folders/p6/nlmq3k8146990kpkxl73mq340000gn/T/RtmpxgEpiK/GradRates_AllParts_2025-12-04.txt

#one part in csv format instead of key-value
produce_gr_report(gr_students, part = "B", format = "readable")
#> Readable results available at  /private/var/folders/p6/nlmq3k8146990kpkxl73mq340000gn/T/RtmpxgEpiK/GradRates_PartB_Readable_2025-12-04.csv
# }
```
