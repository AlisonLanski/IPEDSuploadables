# Output files from the make functions using the package dummy data (for testing)

Contains a list of output files from each "make" script which can be
called by testthat tests to ensure refactoring doesn't change output
inappropriately

## Usage

``` r
part_outputs
```

## Format

A list of dataframes: one dataframe per make script

## Details

General list-creation code is commented out in the test-part-outputs.R
script; uncomment and run/add as needed if a df needs a legitimate
update (e.g. IPEDS rules change, dummy data includes new use case)

## Examples

``` r
part_outputs$com_partD
#> # A tibble: 8 × 20
#>   UNITID SURVSECT PART  CTLEVEL CRACE15 CRACE16 CRACE17 CRACE41 CRACE42 CRACE43
#>   <chr>  <chr>    <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#> 1 999999 COM      D           2       3       0       3       0       0       0
#> 2 999999 COM      D           3       3       3       0       3       0       0
#> 3 999999 COM      D           4       6      15       6       6       0       0
#> 4 999999 COM      D           5       5      12       4       4       0       0
#> 5 999999 COM      D           6       6       9       3       3       0       0
#> 6 999999 COM      D           7       3       0       0       0       0       0
#> 7 999999 COM      D           8       0       0       0       0       0       0
#> 8 999999 COM      D           9       0       0       0       0       0       0
#> # ℹ 10 more variables: CRACE44 <dbl>, CRACE45 <dbl>, CRACE46 <dbl>,
#> #   CRACE47 <dbl>, CRACE23 <dbl>, AGE1 <dbl>, AGE2 <dbl>, AGE3 <dbl>,
#> #   AGE4 <dbl>, AGE5 <dbl>
```
