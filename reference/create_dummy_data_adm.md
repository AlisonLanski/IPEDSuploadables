# Create dummy data for testing the Admissions functions

Creates dummy data for testing the Admissions functions

## Usage

``` r
create_dummy_data_adm(seed = 4567)
```

## Arguments

- seed:

  Number to set a seed to randomize exam scores

## Value

a dataframe of 40 applicants ready for the rest of the Admissions
functions

## Examples

``` r
#use default seed
admissions <- create_dummy_data_adm()

#use custom seed
admissions_scores <- create_dummy_data_adm(seed = 123456)
```
