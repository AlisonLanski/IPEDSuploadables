# Shortcut function with all steps to provide a Completions report

Shortcut function with all steps to provide a Completions report

## Usage

``` r
produce_com_report(
  df,
  extracips = NULL,
  part = "ALL",
  format = "uploadable",
  ugender = lifecycle::deprecated(),
  ggender = lifecycle::deprecated()
)
```

## Arguments

- df:

  A dataframe set up according to the readme

- extracips:

  A dataframe set up according to the readme (optional)

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
produce_com_report(com_students, com_cips)
#> Uploadable results available at /private/var/folders/p6/nlmq3k8146990kpkxl73mq340000gn/T/RtmpxgEpiK/Completions_AllParts_2025-12-04.txt

#one part as csv instead of key-value
produce_com_report(com_students, com_cips, part = "A", format = "readable")
#> Readable results available at  /private/var/folders/p6/nlmq3k8146990kpkxl73mq340000gn/T/RtmpxgEpiK/Completions_PartA_Readable_2025-12-04.csv
# }
```
