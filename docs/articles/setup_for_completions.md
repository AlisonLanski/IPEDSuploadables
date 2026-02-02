# Set up your data for IPEDS Completions processing

## To use the `produce_com_report` function, your student completions data must be in the format described below

> - One row per person per degree level per cip per major
> - One IPEDS Unitid per file
> - Columns with values and types as described below (additional columns
>   are allowed)

Note: Column names can use any capitalization method you like

[TABLE]

### If there are any cip code/degree level/major number combinations that are possible for students to obtain but are NOT represented in this year’s student completions data, use the format below to incorporate them into the `make_com` functions.

> - One row per degree level per cip per major
> - One IPEDS Unitid per file
> - Columns with values and types as described below (additional columns
>   are allowed)

[TABLE]
