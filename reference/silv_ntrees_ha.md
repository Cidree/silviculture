# Calculates number of trees per hectare

**\[deprecated\]** Calculates number of trees per hectare for a given
plot size and shape

## Usage

``` r
silv_ntrees_ha(ntrees, plot_size, plot_shape = "circular")
```

## Arguments

- ntrees:

  A numeric vector representing the number of trees in a sampling plot

- plot_size:

  A numeric vector of length one for circular radius in meters; or a
  numeric vector of length two for each side of a rectangular plot shape

- plot_shape:

  The shape of the sampling plot. Either `circular` or `rectangular`

## Value

A numeric vector

## Examples

``` r
library(dplyr)
## Circular plot of 10 meters radius
inventory_samples |>
  count(plot_id, species) |>
  mutate(
    ntrees_ha = silv_ntrees_ha(n, plot_size = 10)
  )
#> # A tibble: 14 × 4
#>    plot_id species     n ntrees_ha
#>      <int>   <int> <int>     <dbl>
#>  1       7      27    16     509. 
#>  2       8      28     2      63.7
#>  3       8      81     8     255. 
#>  4       8      83     7     223. 
#>  5       8     294     5     159. 
#>  6      10      27     6     191. 
#>  7      10      72     4     127. 
#>  8      10      81    10     318. 
#>  9      10      83     5     159. 
#> 10      53      27    19     605. 
#> 11     189      81    14     446. 
#> 12     189      82    13     414. 
#> 13     189      83    11     350. 
#> 14     189      84    42    1337. 

## Rectangular plot of 10x15 meters
inventory_samples |>
  count(plot_id, species) |>
  mutate(
    ntrees_ha = silv_ntrees_ha(
      n,
      plot_size = c(10, 15),
      plot_shape = "rectangular"
     )
  )
#> # A tibble: 14 × 4
#>    plot_id species     n ntrees_ha
#>      <int>   <int> <int>     <dbl>
#>  1       7      27    16     1067.
#>  2       8      28     2      133.
#>  3       8      81     8      533.
#>  4       8      83     7      467.
#>  5       8     294     5      333.
#>  6      10      27     6      400 
#>  7      10      72     4      267.
#>  8      10      81    10      667.
#>  9      10      83     5      333.
#> 10      53      27    19     1267.
#> 11     189      81    14      933.
#> 12     189      82    13      867.
#> 13     189      83    11      733.
#> 14     189      84    42     2800 
```
