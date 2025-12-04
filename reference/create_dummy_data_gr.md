# Create dummy data for testing the Grad Rates functions

Creates dummy data for testing the Grad Rates functions

## Usage

``` r
create_dummy_data_gr(n = 100)
```

## Arguments

- n:

  Number of rows of data to synthesize

## Value

a dataframe ready for the rest of the Grad Rates functions

## Examples

``` r
#use this seed to reproduce the dummy data saved to the package
set.seed(4567)

#default makes 100 students
graduated <- create_dummy_data_gr()

more_graduated <- create_dummy_data_gr(n = 500)
```
