# Ten years of daily average air temperature

A dataset containing ten years (2001-01-01 - 2010-12-31) of average air
temperature measured at the weather station Goettingen. The data is the
result of a reanalysis of observed data using the STARS model (cf.
Orlowsky et al. 2008). They were provided by Prof. Dr. Peter Werner
(PIK).

## Usage

``` r
data(goe)
```

## Format

A data frame with 3652 rows and 6 variables:

|              |                                       |
|--------------|---------------------------------------|
| **Variable** | **Description**                       |
| date         | calendar date as object of class Date |
| year         | years as integer                      |
| month        | month as integer                      |
| day          | day of month as integer               |
| doy          | day of year as integer                |
| t            | average air temperature               |

## Source

Prof. Dr. Peter Werner (PIK)

## References

Orlowsky, B., Gerstengarbe, F. W., Werner, P. C. (2008) A resampling
scheme for regional climate simulations and its performance compared to
a dynamical RCM. *Theoretical and Applied Climatology*, **92**(3–4),
209–223.
[doi:10.1007/s00704-007-0352-y](https://doi.org/10.1007/s00704-007-0352-y)

## Examples

``` r
data(goe)
str(goe)
#> 'data.frame':    3652 obs. of  6 variables:
#>  $ date : Date, format: "2001-01-01" "2001-01-02" ...
#>  $ year : int  2001 2001 2001 2001 2001 2001 2001 2001 2001 2001 ...
#>  $ month: int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ day  : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ doy  : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ t    : num  6.2 6.5 3.7 3.2 6.8 6.6 5.2 4.7 4.5 4.8 ...
```
