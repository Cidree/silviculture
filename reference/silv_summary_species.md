# Summarize plot data by species

Calculates the number of trees and basal area per hectare for each
species in a plot, and optionally provides the top species by basal
area.

## Usage

``` r
silv_summary_species(
  data,
  plot_id,
  species,
  expan = NULL,
  g = NULL,
  diameter = NULL,
  top_n = 3
)
```

## Arguments

- data:

  A data frame or tibble of tree-level data

- plot_id:

  Unquoted column name with the plot identifier

- species:

  Unquoted column name with the species identifier

- expan:

  Unquoted column name with the expansion factor (trees/ha). If `NULL`,
  it is assumed each row represents 1 tree/ha.

- g:

  Unquoted column name with the basal area per tree (m²/ha). If `NULL`,
  `diameter` must be provided.

- diameter:

  Unquoted column name with the diameter (cm). Used to calculate basal
  area if `g` is `NULL`.

- top_n:

  Number of top species to pivot into columns (default: 3). If `0`, no
  pivoting is performed.

## Value

A tibble with species-level summaries.

## Examples

``` r
library(dplyr)
inventory_samples |>
  mutate(expan = silv_density_ntrees_ha(1, 10)) |>
  silv_summary_species(plot_id, species, expan, diameter = diameter, top_n = 3)
#> # A tibble: 5 × 10
#>   plot_id   sp1   sp2   sp3 G_sp1 G_sp2 G_sp3  N_sp1 N_sp2 N_sp3
#>     <int> <int> <int> <int> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>
#> 1       7    27    NA    NA 135.  NA    NA     509.    NA    NA 
#> 2       8    28    81    83  15.9  4.55  4.05   63.7  255.  223.
#> 3      10    27    72    81 118.  17.2   4.87  191.   127.  318.
#> 4      53    27    NA    NA  88.6 NA    NA     605.    NA    NA 
#> 5     189    84    81    82  46.1 15.8  10.4  1337.   446.  414.
```
