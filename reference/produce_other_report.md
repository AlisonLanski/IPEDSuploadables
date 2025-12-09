# Produce an upload-compatible txt file from pre-aggregated files

Use this function to create a key-value pair uploadable file from your
own prepared dataframes, instead of using a different (provided)
`produce` function. Your dataframes must be prepped to match final
submission requirements as laid out by IPEDS (or whatever survey you
will use this for. Use this function for one survey at a time, and add a
separate dataframe for each part to the `...` argument. See vignette for
more details.

## Usage

``` r
produce_other_report(..., survey = "MySurvey", part = "AllParts")
```

## Arguments

- ...:

  dataframes (one for each survey part, in order)

- survey:

  string with the survey name you'd like in your filename

- part:

  string with the part name (subname) you'd like your file name

## Value

txt file on your computer with the title
*\[survey\]\_\[part\]\_\[today's date\].txt*

## Note

You must name the arguments for `survey` and `part` if using non-default
value. If the arguments are unnamed, the function will assume their
values are additional dataframes.

## Examples

``` r
#With built-in R data
produce_other_report(mtcars[1:5,], iris[1:5,], ToothGrowth[1:5,], survey = 'FakeSurvey')
#> Uploadable results available at /private/var/folders/_l/91ns3hs96sd11p4_lzxh1l140000gn/T/Rtmp0BKbTC/FakeSurvey_AllParts_2025-12-09.txt
# \donttest{
#Will not execute properly (argument unnamed)
#produce_other_report(mtcars[1:5,], iris[1:5,], ToothGrowth[1:5,], 'FakeSurvey')
# }
```
