# Calculates Basal Area

Calculates Basal Area in square meters.

## Usage

``` r
silv_stand_basal_area(diameter, ntrees = NULL, units = "cm")
```

## Arguments

- diameter:

  Numeric vector of diameters or diameter classes

- ntrees:

  Numeric vector with number of trees of the diameter class per hectare.
  If `ntrees = NULL`, the function will assume that each diameter
  corresponds to only one tree

- units:

  The units of the diameter (one of `mm`, `cm`, `dm`, or `m`)

## Value

A numeric vector

## Details

The function uses the next formula:

\\G = \frac{\pi}{40000} \cdot D^2 \cdot \text{ntrees}\\

where G is the basal area in \\m^2\\, and D is the diameter in `cm`. If
ntrees in the number of trees per hectare, then the result will be
\\m^2/ha\\. It is recommended to use the squared mean diameter
calculated with
[`silv_stand_qmean_diameter()`](https://cidree.github.io/silviculture/reference/silv_stand_qmean_diameter.md).

Note that if `ntrees = NULL`, the output of the function will be exactly
the same as in
[`silv_tree_basal_area()`](https://cidree.github.io/silviculture/reference/silv_tree_basal_area.md).

## Examples

``` r
## calculate G for inventory data grouped by plot_id and species
library(dplyr)
inventory_samples |>
mutate(dclass = silv_tree_dclass(diameter)) |>
  summarise(
    height = mean(height, na.rm = TRUE),
    ntrees = n(),
    .by    = c(plot_id, species, dclass)
  ) |>
  mutate(
    ntrees_ha = silv_density_ntrees_ha(ntrees, plot_size = 10),
    dg        = silv_stand_qmean_diameter(dclass, ntrees_ha),
    g         = silv_stand_basal_area(dclass, ntrees_ha),
    .by       = c(plot_id, species)
  )
#> # A tibble: 57 × 8
#>    plot_id species dclass height ntrees ntrees_ha    dg       g
#>      <int>   <int>  <dbl>  <dbl>  <int>     <dbl> <dbl>   <dbl>
#>  1       7      27     50  18         3      95.5  57.9 0.196  
#>  2       7      27     55  17.6       5     159.   57.9 0.238  
#>  3       7      27     35  16.5       1      31.8  57.9 0.0962 
#>  4       7      27     45  14.6       2      63.7  57.9 0.159  
#>  5       7      27     60  19.1       3      95.5  57.9 0.283  
#>  6       7      27     25  12.9       1      31.8  57.9 0.0491 
#>  7       7      27    120  20.9       1      31.8  57.9 1.13   
#>  8       8      83     20   5.10      3      95.5  15.1 0.0314 
#>  9       8      83     10   6.10      4     127.   15.1 0.00785
#> 10       8      28     55  15.5       1      31.8  57.6 0.238  
#> # ℹ 47 more rows
```
