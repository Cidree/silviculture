# Calculates Lorey's Height

\#' @description **\[deprecated\]**

## Usage

``` r
silv_lorey_height(height, g, ntrees = NULL)
```

## Arguments

- height:

  Numeric vector of heights

- g:

  Numeric vector of basal areas

- ntrees:

  Optional. Numeric vector of number of trees per hectare. Use this
  argument when you have aggregated data by diametric classes (see
  details).

## Value

A numeric vector

## Details

Tree's mean height weighted by basal area

The function calculates Lorey's mean height according to:

\$\$h_L = \frac{\sum n_i g_i h_i}{\sum n_i g_i}\$\$

When ntrees is not provided (i.e. `ntrees = NULL`) the formula is simply
the weighted mean of the provided heights and basal areas:

\$\$h_L = \frac{\sum g_i h_i}{\sum g_i}\$\$

## Examples

``` r
## Calculate Lorey's Height by plot and species
library(dplyr)
inventory_samples |>
  mutate(g = silv_basal_area(diameter)) |>
  summarise(
    lh  = silv_lorey_height(height, g),
    .by = c(plot_id, species)
  )
#> Warning: There was 1 warning in `summarise()`.
#> ℹ In argument: `lh = silv_lorey_height(height, g)`.
#> ℹ In group 1: `plot_id = 7`, `species = 27`.
#> Caused by warning:
#> ! `silv_lorey_height()` was deprecated in silviculture 0.2.0.
#> ℹ Function `silv_lorey_height() is deprecated in favour of
#>   `silv_stand_lorey_height()`, and it will be removed in the next release.
#> # A tibble: 14 × 3
#>    plot_id species    lh
#>      <int>   <int> <dbl>
#>  1       7      27 18.5 
#>  2       8      83  5.46
#>  3       8      28 17.7 
#>  4       8     294  6.95
#>  5       8      81  6.14
#>  6      10      81  7.51
#>  7      10      72 14.2 
#>  8      10      83  6.37
#>  9      10      27 28.6 
#> 10      53      27 23.5 
#> 11     189      81 12.6 
#> 12     189      84 12.9 
#> 13     189      82 10.9 
#> 14     189      83  9.49
```
