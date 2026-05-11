# Calculates the dominant height

Calculates the dominant height using the Assman equation or the Hart
equation

## Usage

``` r
silv_stand_dominant_height(diameter, height, ntrees = NULL, which = "assman")
```

## Arguments

- diameter:

  Numeric vector with diameter classes

- height:

  Numeric vector with averaged heights by diameter class

- ntrees:

  Optional. Numeric vector with number of trees per hectare. Use this
  argument when you have aggregated data by diametric classes (see
  details).

- which:

  The method to calculate the dominant height (see details)

## Value

A numeric vector

## Details

The dominant height \\H_0\\ is the mean height of dominant trees, which
is less affected than overall mean height by thinning or other
treatments.

- **Assman**: calculates the \\H_0\\ as the mean height of the 100
  thickest trees per hectare

- **Hart**: calculates the \\H_0\\ as the mean height of the 100 tallest
  trees per hectare

When `ntrees = NULL`, the function will assume that each diameter and
height belongs to only one tree. If you have data aggregated by hectare,
you'll use the number of trees per hectare in this argument.

## References

Assmann, E. (1970) The principles of forest yield study: Studies in the
organic production, structure, increment, and yield of forest stands.
Pergamon Press, Oxford.

## Examples

``` r
## calculate h0 for inventory data grouped by plot_id and species
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
    h0        = silv_stand_dominant_height(dclass, height, ntrees_ha),
    .by       = c(plot_id, species)
  )
#> # A tibble: 57 × 7
#>    plot_id species dclass height ntrees ntrees_ha    h0
#>      <int>   <int>  <dbl>  <dbl>  <int>     <dbl> <dbl>
#>  1       7      27     50  18         3      95.5 19.7 
#>  2       7      27     55  17.6       5     159.  19.7 
#>  3       7      27     35  16.5       1      31.8 19.7 
#>  4       7      27     45  14.6       2      63.7 19.7 
#>  5       7      27     60  19.1       3      95.5 19.7 
#>  6       7      27     25  12.9       1      31.8 19.7 
#>  7       7      27    120  20.9       1      31.8 19.7 
#>  8       8      83     20   5.10      3      95.5  5.15
#>  9       8      83     10   6.10      4     127.   5.15
#> 10       8      28     55  15.5       1      31.8 17.5 
#> # ℹ 47 more rows
```
