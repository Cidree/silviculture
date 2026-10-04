# Tree-level inventory summary

Computes individual tree-level dendrometric metrics, diametric classes,
expansion factors, competition indices, and optional volume, biomass,
and carbon predictions.

## Usage

``` r
silv_tree_summary(
  data,
  diameter,
  height = NULL,
  plot_id = NULL,
  species = NULL,
  expan = NULL,
  plot_size = NULL,
  plot_shape = c("circular", "rectangular", "snfi"),
  dmin = 7.5,
  dmax = NULL,
  class_length = 5,
  include_lowest = TRUE,
  compute_bal = FALSE,
  predict_volume = FALSE,
  province = NULL,
  predict_biomass = FALSE,
  biomass_component = "tree",
  predict_carbon = FALSE
)
```

## Arguments

- data:

  A data frame or tibble with tree-level records.

- diameter:

  Unquoted column name with the tree diameter (in cm).

- height:

  Unquoted column name with the tree height (in m), optional.

- plot_id:

  Unquoted column name with the plot identifier, optional.

- species:

  Unquoted column name with the tree species identifier/name, optional.

- expan:

  Unquoted column name with the expansion factor (trees/ha), optional.

- plot_size:

  Numeric. Size of the sampling plot (radius in meters if circular, area
  in m² if rectangular).

- plot_shape:

  Character. Shape of the sampling plot (`"circular"` or
  `"rectangular"`). Default is `"circular"`.

- dmin:

  Numeric. Minimum diameter for diametric classes (default: 7.5).

- dmax:

  Numeric. Maximum diameter for diametric classes (default: NULL).

- class_length:

  Numeric. Width of diametric classes (default: 5).

- include_lowest:

  Logical. Whether to include lowest bound in classes (default: TRUE).

- compute_bal:

  Logical. If TRUE and `plot_id` is supplied, computes BAL and BAS
  (default: FALSE).

- predict_volume:

  Logical. If TRUE, predicts SNFI volume (vcc, vsc, iavc) using
  [`silv_predict_snfi_volume()`](https://cidree.github.io/silviculture/reference/silv_predict_snfi_volume.md)
  (default: FALSE).

- province:

  Unquoted column name or scalar string/integer with the province
  code/name for volume prediction.

- predict_biomass:

  Logical. If TRUE, predicts tree biomass using
  [`silv_predict_biomass_auto()`](https://cidree.github.io/silviculture/reference/silv_predict_biomass_auto.md)
  (default: FALSE).

- biomass_component:

  Character. Tree component to predict for biomass (default: `"tree"`).

- predict_carbon:

  Logical. If TRUE, predicts tree carbon content using
  [`silv_predict_carbon_auto()`](https://cidree.github.io/silviculture/reference/predict_carbon.md)
  (default: FALSE).

## Value

The original data frame enriched with computed tree-level metrics.

## Examples

``` r
library(dplyr)
silv_tree_summary(
  data       = inventory_samples,
  diameter   = diameter,
  height     = height,
  plot_id    = plot_id,
  species    = species,
  plot_size  = 10,
  compute_bal = TRUE
)
#> # A tibble: 162 × 11
#>    plot_id species diameter height dclass      g expan  g_ha slenderness   bal
#>      <int>   <int>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>       <dbl> <dbl>
#>  1       7      27     50.6   18.9     50 0.201   31.8  6.40        37.4 101. 
#>  2       7      27     57.2   19.8     55 0.257   31.8  8.18        34.6  62.6
#>  3       7      27     36.4   16.5     35 0.104   31.8  3.31        45.3 130. 
#>  4       7      27     46.4   18.5     45 0.169   31.8  5.38        39.9 125. 
#>  5       7      27     55.5   19.5     55 0.242   31.8  7.70        35.1  70.8
#>  6       7      27     59.5   17.7     60 0.278   31.8  8.85        29.7  45.1
#>  7       7      27     24.3   12.9     25 0.0464  31.8  1.48        53.1 134. 
#>  8       7      27     50.5   16.6     50 0.200   31.8  6.38        32.9 107. 
#>  9       7      27     55.3   19.3     55 0.240   31.8  7.65        34.9  78.5
#> 10       7      27     48.6   18.5     50 0.186   31.8  5.90        38.1 114. 
#> # ℹ 152 more rows
#> # ℹ 1 more variable: bas <dbl>
```
