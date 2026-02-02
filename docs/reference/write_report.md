# Write the prepared data to a txt file in key-value format

Write the prepared data to a txt file in key-value format

## Usage

``` r
write_report(..., survey, part, output_path)
```

## Arguments

- ...:

  dataframes (one for each survey part, in order)

- survey:

  a string (which \[IPEDS\] survey)

- part:

  a string (which upload part of the survey)

- output_path:

  a file path (where the file should be saved)

## Value

a txt file (at the path location)

## Note

All arguments for this function are required and must be named.
Dataframes must have the key as the column name (with appropriate
capitalization) and the value in the cells
