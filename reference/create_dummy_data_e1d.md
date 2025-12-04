# Create dummy data for testing the completions functions

Creates a prepared dataframe to test scripts related to IPEDS 12 Month
Enrollment reporting. Produces either a student dataframe or a dataframe
of instructional activity, depending on the argument you select

## Usage

``` r
create_dummy_data_e1d(df_type = "student")
```

## Arguments

- df_type:

  a string: "student" to get the main df needed, "instr" to get
  instructionalactivity

## Value

a dataframe ready for the rest of the e1d scripts

## Note

The final dataset has 100 students 60 UG students (40 FT, 20 PT; 26
seeking degrees, 34 not) UG include: 20 first time, 20 transfer, 20
continuing/returning; 40 Grad Students (10 FT, 30 PT; 24 seeking
degrees, 16 not)

For simplicity, only 1 race-ethnicity category is used 5 UG and 5 Grad
are set to be fully distance ed 10 UG are set to be partially distance
ed

## Examples

``` r
set.seed(1892)

student_df <- create_dummy_data_e1d()

instr_df <- create_dummy_data_e1d(df_type = "instr")
```
