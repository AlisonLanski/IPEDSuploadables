# Create dummy data for testing the completions functions

Creates a prepared dataframe to test scripts related to IPEDS
Completions reporting. Produces either a student/degree dataframe or a
dataframe of cips previously reported but not in the current student
data, depending on the argument you select

## Usage

``` r
create_dummy_data_com(df_type = "student")
```

## Arguments

- df_type:

  a string: "student" to get the main df needed, "cip" to get extracips

## Value

a dataframe ready for the rest of the comp scripts

## Note

The final dataset has 60 students with 105 majors. Students 100-130,
140, 150 have 1 major for 1 degree (journalism) Students 131-139 have 2
majors for 1 degree (journalism + parks) Students 141-149 have 3 majors
for 1 degree (journalism, parks, linguistics) Students 151-159 have 3
majors for 2 degrees (1 degree with journalism/parks, 1 MBA degree)
Note: 1 student has a faulty birthdate; this will show the warning "1
failed to parse"

Two rows (level 18 linguistics) are flagged as distance education

To fully process completions, we will need to include an example of a
CIP code that is a possible major but has no completers and a CIP code
in an award level that is possible but has no completers This is the
second piece of dummy df produced

## Examples

``` r
set.seed(1892)

# one date fails to parse:
# this is to provide an example of missing
# data which is acceptable to IPEDS
students <- create_dummy_data_com()
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `Birthdate = lubridate::ymd(...)`.
#> Caused by warning:
#> !  1 failed to parse.

additional_cips <- create_dummy_data_com(df_type = "cip")
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `Birthdate = lubridate::ymd(...)`.
#> Caused by warning:
#> !  1 failed to parse.
```
