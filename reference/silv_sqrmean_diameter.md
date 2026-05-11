# Calculates the quadratic mean diameter (QMD)

**\[deprecated\]**

## Usage

``` r
silv_sqrmean_diameter(diameter, ntrees = NULL)
```

## Arguments

- diameter:

  Numeric vector of diameters or diameter classes

- ntrees:

  Numeric vector with number of trees of the diameter class per hectare.
  If `ntrees = NULL`, the function will assume that each diameter
  corresponds to only one tree. Therefore, the QMD will be calculated
  for each individual tree

## Value

A numeric vector

## Examples

``` r
## calculate dg for inventory data grouped by plot_id and species
library(dplyr)
inventory_samples |>
mutate(dclass = silv_diametric_class(diameter)) |>
  summarise(
    height = mean(height, na.rm = TRUE),
    ntrees = n(),
    .by    = c(plot_id, species, dclass)
  ) |>
  mutate(
    ntrees_ha = silv_ntrees_ha(ntrees, plot_size = 10),
    h0        = silv_dominant_height(dclass, height, ntrees_ha),
    dg        = silv_sqrmean_diameter(dclass, ntrees_ha),
    .by       = c(plot_id, species)
  )
#> # A tibble: 57 × 8
#>    plot_id species dclass height ntrees ntrees_ha    h0    dg
#>      <int>   <int>  <dbl>  <dbl>  <int>     <dbl> <dbl> <dbl>
#>  1       7      27     50  18         3      95.5 19.7   57.9
#>  2       7      27     55  17.6       5     159.  19.7   57.9
#>  3       7      27     35  16.5       1      31.8 19.7   57.9
#>  4       7      27     45  14.6       2      63.7 19.7   57.9
#>  5       7      27     60  19.1       3      95.5 19.7   57.9
#>  6       7      27     25  12.9       1      31.8 19.7   57.9
#>  7       7      27    120  20.9       1      31.8 19.7   57.9
#>  8       8      83     20   5.10      3      95.5  5.15  15.1
#>  9       8      83     10   6.10      4     127.   5.15  15.1
#> 10       8      28     55  15.5       1      31.8 17.5   57.6
#> # ℹ 47 more rows

## calculate dg for a vector of diameters
silv_sqrmean_diameter(c(12.5, 23.5, 14, 16, 18.5))
#> [1] 17.33638
```
