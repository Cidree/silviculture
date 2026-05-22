# Calculates Lorey's Height

Tree's mean height weighted by basal area

## Usage

``` r
silv_stand_lorey_height(height, g, ntrees = NULL)
```

## Arguments

- height:

  Numeric vector of tree heights

- g:

  Numeric vector of basal areas

- ntrees:

  Numeric vector with number of trees of the diameter class per hectare.
  If `ntrees = NULL`, the function will assume that each diameter
  corresponds to only one tree

## Value

A numeric vector

## Details

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
  mutate(g = silv_tree_basal_area(diameter)) |>
  summarise(
    lh  = silv_stand_lorey_height(height, g),
    .by = c(plot_id, species)
  )
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
