# Shortcut function with all steps to provide a Fall Enrollment report

Shortcut function with all steps to provide a Fall Enrollment report

## Usage

``` r
produce_ef1_report(
  students,
  retention,
  part = "ALL",
  include_optional = FALSE,
  format = "uploadable",
  ugender = lifecycle::deprecated(),
  ggender = lifecycle::deprecated()
)
```

## Arguments

- students:

  A dataframe set up according to the readme with student data

- retention:

  A dataframe set up according to the readme with retention data

- part:

  A string with what part of the report you want to produce: 'all', 'A',
  etc.

- include_optional:

  A boolean flag for whether optional parts should be included

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
produce_ef1_report(ef1_students, ef1_retention)
#> Uploadable results available at /private/var/folders/yz/zr09txvs5dn18vt4cn21kzl40000gn/T/RtmpcywHSW/FallEnrollment_AllParts_2026-02-02.txt

#entire report with optional sections
produce_ef1_report(ef1_students, ef1_retention, include_optional = TRUE)
#> Uploadable results available at /private/var/folders/yz/zr09txvs5dn18vt4cn21kzl40000gn/T/RtmpcywHSW/FallEnrollment_AllParts_2026-02-02.txt

#one part as csv instead of key-value
produce_ef1_report(ef1_students, part = 'D', format = 'readable')
#> Readable results available at  /private/var/folders/yz/zr09txvs5dn18vt4cn21kzl40000gn/T/RtmpcywHSW/FallEnrollment_PartD_Readable_2026-02-02.csv
# }
```
