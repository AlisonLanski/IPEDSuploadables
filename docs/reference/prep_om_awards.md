# Set up extra_awards df for Outcome Measures part B, C, D

Select correct year, ensure all award levels end up with a column

## Usage

``` r
prep_om_awards(df, award)
```

## Arguments

- df:

  A dataframe of student statuses

- award:

  A string with the df column to use for processing depending on the OM
  part

## Value

A dataframe pivoted and prepared for use within the make_om_part
functions B-D
