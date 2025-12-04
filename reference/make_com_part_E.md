# Make Completions Part E (gender details)

Make Completions Part E (gender details)

## Usage

``` r
make_com_part_E(
  df,
  ugender = lifecycle::deprecated(),
  ggender = lifecycle::deprecated()
)
```

## Arguments

- df:

  A dataframe of student/degree information

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

A dataframe with the required IPEDS structure for this survey part
