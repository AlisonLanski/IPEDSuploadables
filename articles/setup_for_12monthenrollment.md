# Set up your data for IPEDS 12 Month Enrollment processing

## To use the `produce_e1d_report` function, you must provide two (2) dataframes: one with student enrollment data and one with instructional activity data.

### The student enrollment data must be in the format described below

> - One row per person
> - One IPEDS Unitid per file
> - Columns with values and types as described below (additional columns
>   are allowed)

Note: Column names can use any capitalization method you like

[TABLE]

### The instructional activity data must be in the format described below

> - One row total, with a column for each reportable fact
> - One IPEDS Unitid per file
> - Columns with values and types as described below (additional columns
>   are allowed)

| Column Name   | Column Type | Acceptable Value - Definition                                                                                                                                                                               |
|:--------------|:------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Unitid        | Numeric     | (relevant unitid; xxxxxx)                                                                                                                                                                                   |
| CreditHoursUG | Numeric     | Number representing total UG credit hours (see IPEDS instructions for details). Use 0 if your school does not use the credit-hours method.                                                                  |
| ClockHoursUG  | Numeric     | Number representing total UG clock hours (see IPEDS instructions for details). Use 0 if your school does not use the clock-hours method.                                                                    |
| CreditHoursGR | Numeric     | Whole number representing total Graduate Student credit hours, omitting doctoral/professional hours (see IPEDS instructions for details). Use 0 if your school does not have graduate student credit hours. |
| DocFTE        | Numeric     | Total FTE of doctoral/professional students (see IPEDS instructions for details. Use 0 if you do not have any.                                                                                              |
