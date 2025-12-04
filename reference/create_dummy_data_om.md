# Create dummy data for testing the outcome measures functions

Creates a prepared dataframe to test scripts related to IPEDS Outcome
Measures reporting. Produces either a student/status dataframe

## Usage

``` r
create_dummy_data_om()
```

## Value

a dataframe ready for the rest of the om scripts

## Details

remember: want to save this data out into the package so it's available

## Note

The final dataset has 20 students covering most statuses

## Examples

``` r
#creates a very specific population
#function does not allow for anything to be updated at time of run
#in other words: will always create a fixed-value dataframe
dat <- create_dummy_data_om()
```
