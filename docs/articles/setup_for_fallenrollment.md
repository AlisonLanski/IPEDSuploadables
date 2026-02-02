# Set up your data for IPEDS Fall Enrollment processing

## The `produce_ef1_report` function requires 2 separate dataframes, one with row-level students and one with pre-aggregated retention counts.

## *The dataframes must follow the format described below*

### Student data requirements

> - One row per person
> - One IPEDS Unitid per file
> - Columns with values and types as described below in any order
>   (additional columns are allowed)
> - Double-starred column names (\*\*) are only required in years when
>   IPEDS collects that information. In off-years, you can omit the
>   columns or enter any placeholder value.

Column names can use any capitalization method you like. Do **NOT**
include (\*\*) in the column names.

[TABLE]

### Retention data requirements

> - One row for full-time counts, one row for part-time counts
> - One IPEDS Unitid per file
> - Columns with values and types as described below in any order
>   (additional columns are allowed)

[TABLE]

### Student-faculty ratio

This information will be collected via pop-up box. Type the correct
number for your institution’s ratio when prompted.
