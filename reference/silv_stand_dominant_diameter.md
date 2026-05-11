# Calculates the dominant diameter

Calculates the dominant diameter using Assman and Friedrich method, or
Weise method

## Usage

``` r
silv_stand_dominant_diameter(
  diameter,
  ntrees = NULL,
  which = "assman",
  quiet = FALSE
)
```

## Arguments

- diameter:

  Numeric vector with diameter classes

- ntrees:

  Optional. Numeric vector with number of trees per hectare. Use this
  argument when you have aggregated data by diametric classes (see
  details).

- which:

  The method to calculate the dominant diameter (see details)

- quiet:

  if `TRUE`, messages will be supressed

## Value

A numeric vector

## Details

The dominant diameter \\D_0\\ is the mean diameter of the 100 thickest
trees per hectare. Therefore, `diameter` and `ntrees` should be vectors
of the same length.

- **Assman**: calculates the \\D_0\\ as the mean diameter of the 100
  thickest trees per hectare

- **Weise**: calculates the \\D_0\\ as the quadratic mean diameter of
  the 20% thickest trees per hectare

## Examples

``` r
## calculate d0 for inventory data grouped by plot_id and species
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
    d0        = silv_stand_dominant_diameter(dclass, ntrees_ha),
    .by       = c(plot_id, species)
  )
#> # A tibble: 57 × 7
#>    plot_id species dclass height ntrees ntrees_ha    d0
#>      <int>   <int>  <dbl>  <dbl>  <int>     <dbl> <dbl>
#>  1       7      27     50  18         3      95.5  79.1
#>  2       7      27     55  17.6       5     159.   79.1
#>  3       7      27     35  16.5       1      31.8  79.1
#>  4       7      27     45  14.6       2      63.7  79.1
#>  5       7      27     60  19.1       3      95.5  79.1
#>  6       7      27     25  12.9       1      31.8  79.1
#>  7       7      27    120  20.9       1      31.8  79.1
#>  8       8      83     20   5.10      3      95.5  19.5
#>  9       8      83     10   6.10      4     127.   19.5
#> 10       8      28     55  15.5       1      31.8  57.5
#> # ℹ 47 more rows
```
