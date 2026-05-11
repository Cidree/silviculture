# Calculates Basal Area

**\[deprecated\]**

Calculates Basal Area in square meters.

## Usage

``` r
silv_basal_area(diameter, ntrees = NULL, units = "cm")
```

## Arguments

- diameter:

  Numeric vector of diameters or diameter classes

- ntrees:

  Numeric vector with number of trees of the diameter class per hectare.
  If `ntrees = NULL`, the function will assume that each diameter
  corresponds to only one tree. Therefore, basal area will be calculated
  for each individual tree

- units:

  The units of the diameter (one of `cm`, `mm`, or `m`)

## Value

A numeric vector

## Details

The function uses the next formula:

\\G = \frac{\pi}{40000} \cdot D^2\\

where G is the basal area in \\m^2\\, and D is the diameter in the
`units` specified in the function. It is recommended to use the squared
mean diameter calculated with
[silv_sqrmean_diameter](https://cidree.github.io/silviculture/reference/silv_sqrmean_diameter.md)

## Examples

``` r
## calculate G for inventory data grouped by plot_id and species
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
inventory_samples |>
mutate(dclass = silv_diametric_class(diameter)) |>
  summarise(
    height = mean(height, na.rm = TRUE),
    ntrees = n(),
    .by    = c(plot_id, species, dclass)
  ) |>
  mutate(
    ntrees_ha = silv_ntrees_ha(ntrees, plot_size = 10),
    dg        = silv_sqrmean_diameter(dclass, ntrees_ha),
    g         = silv_basal_area(dclass, ntrees_ha),
    .by       = c(plot_id, species)
  )
#> Warning: There was 1 warning in `mutate()`.
#> ℹ In argument: `dclass = silv_diametric_class(diameter)`.
#> Caused by warning:
#> ! `silv_diametric_class()` was deprecated in silviculture 0.2.0.
#> ℹ Function `silv_diametric_class() is deprecated in favour of
#>   `silv_tree_dclass()`, and it will be removed in the next release.
#> Warning: There were 3 warnings in `mutate()`.
#> The first warning was:
#> ℹ In argument: `ntrees_ha = silv_ntrees_ha(ntrees, plot_size = 10)`.
#> ℹ In group 1: `plot_id = 7`, `species = 27`.
#> Caused by warning:
#> ! `silv_ntrees_ha()` was deprecated in silviculture 0.2.0.
#> ℹ Function `silv_ntrees_ha() is deprecated in favour of
#>   `silv_density_ntrees_ha()`, and it will be removed in the next release.
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 2 remaining warnings.
#> # A tibble: 57 × 8
#>    plot_id species dclass height ntrees ntrees_ha    dg     g
#>      <int>   <int>  <dbl>  <dbl>  <int>     <dbl> <dbl> <dbl>
#>  1       7      27     50  18         3      95.5  57.9 18.8 
#>  2       7      27     55  17.6       5     159.   57.9 37.8 
#>  3       7      27     35  16.5       1      31.8  57.9  3.06
#>  4       7      27     45  14.6       2      63.7  57.9 10.1 
#>  5       7      27     60  19.1       3      95.5  57.9 27   
#>  6       7      27     25  12.9       1      31.8  57.9  1.56
#>  7       7      27    120  20.9       1      31.8  57.9 36   
#>  8       8      83     20   5.10      3      95.5  15.1  3   
#>  9       8      83     10   6.10      4     127.   15.1  1   
#> 10       8      28     55  15.5       1      31.8  57.6  7.56
#> # ℹ 47 more rows

## calculate individual basal area
silv_basal_area(c(23, 11, 43.5, 94))
#> [1] 0.041547563 0.009503318 0.148616967 0.693977817
```
