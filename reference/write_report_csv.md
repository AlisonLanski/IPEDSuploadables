# Write the prepared data to a csv file

Write the prepared data to a csv file

## Usage

``` r
write_report_csv(df, survey, part, output_path)
```

## Arguments

- df:

  a dataframe (prepared via the 'make' scripts)

- survey:

  a string (which IPEDS survey)

- part:

  a string (which upload part of the survey)

- output_path:

  a path (which folder the report should go in)

## Value

a csv file (at the path location)

## Note

All arguments for this function are required. The dataframe must have
the key as the column name (with appropriate capitalization) and the
value in the cells
