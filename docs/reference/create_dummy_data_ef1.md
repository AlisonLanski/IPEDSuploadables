# Create dummy data for testing the fall enrollment functions

Creates students and retention dataframes for use in parts A, B, C, D,
E, G, H. Student-faculty ratio (part G) will ask for a number when the
function is run and does not need to exist here. To create both
dataframes, run the function twice with different arguments, and save
results into separate objects.

## Usage

``` r
create_dummy_data_ef1(df_type = "students", n = 100)
```

## Arguments

- df_type:

  A string with the dummy data requested ("students" for parts A-D & G-H
  or "retention" for part E)

- n:

  A number

## Value

A dataframe ready for the rest of the ef1 scripts

## Examples

``` r
set.seed(1234)

#default creates 100 students
students <- create_dummy_data_ef1()

#change the dataframe
retention <- create_dummy_data_ef1(df_type = "retention")

#change the population size
more_students <- create_dummy_data_ef1(df_type = "students", n = 250)
```
