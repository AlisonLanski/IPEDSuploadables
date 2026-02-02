# Make 12 Month Enrollment Part D (gender details)

Make 12 Month Enrollment Part D (gender details)

## Usage

``` r
make_e1d_part_D(
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
  students, even if you have no (or few) such students. Set as FALSE if
  necessary. \*\*Starting in 2025-2026, this argument will be ignored by
  later code.\*\*

- ggender:

  \`r lifecycle::badge("deprecated")\` A boolean: TRUE means you are
  collecting and able to report "another gender" for graduate students,
  even if you have no (or few) such students. Set as FALSE if necessary.
  \*\*Starting in 2025-2026, this argument will be ignored by later
  code.\*\*

## Value

A dataframe with the required IPEDS structure for this survey part
