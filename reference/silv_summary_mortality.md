# Summarize plot data by mortality status

Calculates the number of trees and basal area per hectare for live and
dead trees in a plot.

## Usage

``` r
silv_summary_mortality(
  data,
  plot_id,
  dead,
  expan = NULL,
  g = NULL,
  diameter = NULL
)
```

## Arguments

- data:

  A data frame or tibble of tree-level data

- plot_id:

  Unquoted column name with the plot identifier

- dead:

  Unquoted column name indicating if the tree is dead (e.g., TRUE/FALSE
  or 1/0)

- expan:

  Unquoted column name with the expansion factor (trees/ha). If `NULL`,
  it is assumed each row represents 1 tree/ha.

- g:

  Unquoted column name with the basal area per tree (m²/ha). If `NULL`,
  `diameter` must be provided.

- diameter:

  Unquoted column name with the diameter (cm). Used to calculate basal
  area if `g` is `NULL`.

## Value

A tibble with plot-level mortality summaries.

## Examples

``` r
library(dplyr)
inventory_samples |>
  mutate(expan = silv_density_ntrees_ha(1, 10)) |>
  mutate(is_dead = sample(c(TRUE, FALSE), n(), replace = TRUE, prob = c(0.1, 0.9))) |>
  silv_summary_mortality(plot_id, is_dead, expan, diameter = diameter)
#> # A tibble: 5 × 5
#>   plot_id N_alive N_dead G_alive G_dead
#>     <int>   <dbl>  <dbl>   <dbl>  <dbl>
#> 1       7    509.    0     135.    0   
#> 2       8    605.   95.5    25.5   1.55
#> 3      10    732.   63.7   139.    3.68
#> 4      53    573.   31.8    82.0   6.60
#> 5     189   2196.  350.     67.0  11.1 
```
