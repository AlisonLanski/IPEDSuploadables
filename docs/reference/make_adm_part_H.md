# Make Admissions Part H (Transfer SAT/ACT scores)

If SAT or ACT scores were used for admission decisions it calculates the
requisite percentiles for submission.

## Usage

``` r
make_adm_part_H(df, ptype = 7)
```

## Arguments

- df:

  A dataframe of applicant information

- ptype:

  (Optional) An integer \[1-9\] indicating which calculation method to
  use for percentiles. The default value within R and here is 7. To see
  details, run [`?quantile`](https://rdrr.io/r/stats/quantile.html) and
  scroll down to "Type".

## Value

Admissions Part H data with the required IPEDS structure
