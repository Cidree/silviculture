# Calculate Forestry Thinning Schemes

Calculates thinning schemes for forest management by selecting trees to
extract based on specified criteria. Supports both thinning from below
(removing smaller trees) and thinning from above (removing larger trees)
approaches.

## Usage

``` r
silv_treatment_thinning(
  data,
  var,
  diameter,
  ntrees,
  thinning = c("below", "above"),
  perc = 0.3,
  .groups = NULL
)
```

## Arguments

- data:

  A data frame, or silviculture::Inventory object. See details.

- var:

  A variable used for calculating the thinning. Typically used variables
  basal area, number of trees, or volume

- diameter:

  Numeric vector of diameters or diameter classes

- ntrees:

  Numeric vector with number of trees of the diameter class per hectare.
  If `ntrees = NULL`, the function will assume that each diameter
  corresponds to only one tree

- thinning:

  Charater string specifying the thinning type. Available options are
  `below` and `above`

- perc:

  Numeric value between 0 and 1 specifying the percentage of `var` to
  extract

- .groups:

  A character vector with variables to group by (e.g. plot id, tree
  species, etc). Ignored when using a `silviculture::Inventory` object

## Value

A `silviculture::Thinning` object with three items:

- **data**: the input data with two new columns

- **group_metrics**: it will include the data from the
  `silviculture::Inventory` object

- **thinning_opts**: options used for S7 methods

## Details

This function implements common silvicultural thinning practices:

**Thinning from below:** Removes trees with the lowest values of the
specified variable. This approach typically removes suppressed, damaged,
or poor-quality trees, mimicking natural mortality processes.

**Thinning from above:** Removes trees with the highest values of the
specified variable. This approach harvests the most valuable trees while
leaving smaller trees to continue growing.

The function calculates which trees to extract based on the ranking of
the specified variable and the desired thinning percentage. When
grouping variables are provided, thinning is calculated separately for
each group.

**Using a silviculture::Inventory object** The result of
[`silv_summary()`](https://cidree.github.io/silviculture/reference/silv_summary.md)
can be used as the `data` argument. If so, the `.groups` will be taken
from this object, and it will keep the previous data in a new S7 object.

## See also

[`silv_summary()`](https://cidree.github.io/silviculture/reference/silv_summary.md)

## Examples

``` r
# Get summary of inventory data
inventory <- inventory_samples |>
 silv_summary(
   diameter  = diameter,
   height    = height,
   plot_size = 25,
   .groups   = c('plot_id', 'species')
 )

## Thinning from below removing 30% of trees based on basal area
silv_treatment_thinning(
  data     = inventory,
  var      = g_ha,
  diameter = dclass,
  ntrees   = ntrees_ha,
  thinning = "below",
  perc     = 0.3
)
#> <silviculture::Thinning>
#>  @ data         : tibble [57 × 11] (S3: tbl_df/tbl/data.frame)
#>  $ plot_id          : int [1:57] 7 7 7 7 7 7 7 8 8 8 ...
#>  $ species          : int [1:57] 27 27 27 27 27 27 27 28 28 81 ...
#>  $ dclass           : num [1:57] 25 35 45 50 55 60 120 55 60 10 ...
#>  $ height           : num [1:57] 12.9 16.5 14.6 18 17.6 ...
#>  $ ntrees           : int [1:57] 1 1 2 3 5 3 1 1 1 3 ...
#>  $ ntrees_ha        : num [1:57] 5.09 5.09 10.19 15.28 25.46 ...
#>  $ h0               : num [1:57] 17.4 17.4 17.4 17.4 17.4 ...
#>  $ dg               : num [1:57] 57.9 57.9 57.9 57.9 57.9 ...
#>  $ g_ha             : num [1:57] 0.25 0.49 1.62 3 6.05 4.32 5.76 1.21 1.44 0.12 ...
#>  $ g_ha_extract     : num [1:57] 0.25 0.49 1.62 3 1.09 ...
#>  $ ntrees_ha_extract: num [1:57] 5.09 5.09 10.19 15.28 4.58 ...
#>  @ group_metrics: tibble [14 × 15] (S3: tbl_df/tbl/data.frame)
#>  $ plot_id  : int [1:14] 7 8 8 8 8 10 10 10 10 53 ...
#>  $ species  : int [1:14] 27 28 81 83 294 27 72 81 83 27 ...
#>  $ d_mean   : num [1:14] 54.7 57.5 15 14.3 14 ...
#>  $ d_median : num [1:14] 55 55 15 10 15 85 35 15 15 40 ...
#>  $ d_sd     : num [1:14] 19.16 2.5 6.12 4.95 2 ...
#>  $ dg       : num [1:14] 57.9 57.6 16.2 15.1 14.1 ...
#>  $ h_mean   : num [1:14] 17.42 17.5 6.29 5.67 6.74 ...
#>  $ h_median : num [1:14] 17.64 15.5 5.87 6.1 7.12 ...
#>  $ h_sd     : num [1:14] 1.924 2 0.525 0.495 0.77 ...
#>  $ h_lorey  : num [1:14] 18.1 17.67 6.43 5.41 7.07 ...
#>  $ h0       : num [1:14] 17.42 17.5 6.29 5.67 6.74 ...
#>  $ ntrees   : int [1:14] 16 2 8 7 5 6 4 10 5 19 ...
#>  $ ntrees_ha: num [1:14] 81.5 10.2 40.7 35.7 25.5 ...
#>  $ g_ha     : num [1:14] 21.49 2.65 0.84 0.64 0.4 ...
#>  $ spacing  : num [1:14] 63.6 179 249.2 295.3 294 ...
#>  @ thinning_opts:List of 5
#>  .. $ var_name   : chr "g_ha_extract"
#>  .. $ dclass_name: chr "dclass"
#>  .. $ thinning   : chr "below"
#>  .. $ percentage : num 0.3
#>  .. $ groups     : chr [1:2] "plot_id" "species"

## Thinning from above removing 20% of trees based on basal area
silv_treatment_thinning(
  data     = inventory,
  var      = g_ha,
  diameter = dclass,
  ntrees   = ntrees_ha,
  thinning = "above",
  perc     = 0.2
)
#> <silviculture::Thinning>
#>  @ data         : tibble [57 × 11] (S3: tbl_df/tbl/data.frame)
#>  $ plot_id          : int [1:57] 7 7 7 7 7 7 7 8 8 8 ...
#>  $ species          : int [1:57] 27 27 27 27 27 27 27 28 28 81 ...
#>  $ dclass           : num [1:57] 120 60 55 50 45 35 25 60 55 30 ...
#>  $ height           : num [1:57] 20.9 19.1 17.6 18 14.6 ...
#>  $ ntrees           : int [1:57] 1 3 5 3 2 1 1 1 1 1 ...
#>  $ ntrees_ha        : num [1:57] 5.09 15.28 25.46 15.28 10.19 ...
#>  $ h0               : num [1:57] 17.4 17.4 17.4 17.4 17.4 ...
#>  $ dg               : num [1:57] 57.9 57.9 57.9 57.9 57.9 ...
#>  $ g_ha             : num [1:57] 5.76 4.32 6.05 3 1.62 0.49 0.25 1.44 1.21 0.36 ...
#>  $ g_ha_extract     : num [1:57] 4.3 0 0 0 0 ...
#>  $ ntrees_ha_extract: num [1:57] 3.8 0 0 0 0 ...
#>  @ group_metrics: tibble [14 × 15] (S3: tbl_df/tbl/data.frame)
#>  $ plot_id  : int [1:14] 7 8 8 8 8 10 10 10 10 53 ...
#>  $ species  : int [1:14] 27 28 81 83 294 27 72 81 83 27 ...
#>  $ d_mean   : num [1:14] 54.7 57.5 15 14.3 14 ...
#>  $ d_median : num [1:14] 55 55 15 10 15 85 35 15 15 40 ...
#>  $ d_sd     : num [1:14] 19.16 2.5 6.12 4.95 2 ...
#>  $ dg       : num [1:14] 57.9 57.6 16.2 15.1 14.1 ...
#>  $ h_mean   : num [1:14] 17.42 17.5 6.29 5.67 6.74 ...
#>  $ h_median : num [1:14] 17.64 15.5 5.87 6.1 7.12 ...
#>  $ h_sd     : num [1:14] 1.924 2 0.525 0.495 0.77 ...
#>  $ h_lorey  : num [1:14] 18.1 17.67 6.43 5.41 7.07 ...
#>  $ h0       : num [1:14] 17.42 17.5 6.29 5.67 6.74 ...
#>  $ ntrees   : int [1:14] 16 2 8 7 5 6 4 10 5 19 ...
#>  $ ntrees_ha: num [1:14] 81.5 10.2 40.7 35.7 25.5 ...
#>  $ g_ha     : num [1:14] 21.49 2.65 0.84 0.64 0.4 ...
#>  $ spacing  : num [1:14] 63.6 179 249.2 295.3 294 ...
#>  @ thinning_opts:List of 5
#>  .. $ var_name   : chr "g_ha_extract"
#>  .. $ dclass_name: chr "dclass"
#>  .. $ thinning   : chr "above"
#>  .. $ percentage : num 0.2
#>  .. $ groups     : chr [1:2] "plot_id" "species"
```
