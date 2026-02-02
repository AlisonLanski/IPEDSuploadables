# Make Graduation Rates Part E (gender details)

\`r lifecycle::badge("deprecated")\`

This section was removed by IPEDS in the 2024-2025 reporting cycle,
after being required for the 2023-2024 reporting cycle. It remains here
in case it is re-added in a future year. I have updated testing/produce
code so nothing else calls it. By making it internal, it won't be listed
but remains available.

## Usage

``` r
make_gr_part_E(df, ugender = lifecycle::deprecated())
```

## Arguments

- df:

  A dataframe of student/degree information for unduplicated
  undergraduates

- ugender:

  \`r lifecycle::badge("deprecated")\` A boolean: TRUE means you are
  collecting and able to report "another gender" for undergraduate
  students, even if you have no (or few) such students. Set as FALSE if
  necessary. \*\*Starting in 2025-2026, this argument will be ignored by
  later code.\*\*

## Value

A dataframe with the required IPEDS structure for this survey part
