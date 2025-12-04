# Set up your data for IPEDS HR processing

## To use the `produce_hr_report` function, your data must be in the format described below

> - One row per person
> - One IPEDS Unitid per file
> - Columns with values and types as described below (additional columns
>   are allowed)

### Note

Starting in the 2022-2023 reporting cycle, we only report New Hires who
are still 11/1 Current Employees. In other words, everyone in your
starting data should have a CurrentEmployee flag of 1. In future years,
we may drop the requirement of the CurrentEmployee column since it no
longer carries valuable information. For this year, we have left it to
avoid further structural changes.

[TABLE]

Notes

- In OccCategory3, `21` is used for an intermediate total and is not a
  value in the upload file  
- OccCategory3 values will be recoded in `prep_hr_data_frame` into all
  necessary rollups, including instructional staff (values 1-4)
