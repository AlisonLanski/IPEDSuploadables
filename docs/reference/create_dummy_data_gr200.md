# Create dummy data for testing the Grad Rates 200 function

Dummy data for Grad Rates 200 testing

## Usage

``` r
create_dummy_data_gr200(n = 1000)
```

## Arguments

- n:

  A number that will be used as the length of the data frame

## Value

a dataframe ready for the rest of the Grad Rates 200 functions

## Examples

``` r
set.seed(4567)

#default creates 1000 students
graduates <- create_dummy_data_gr200()
more_graduates <- create_dummy_data_gr200(n = 100)
```
