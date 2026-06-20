# Calculates a bunch of forest metrics

Summarize forest inventory data calculating most typical variables

## Usage

``` r
silv_summary(
  data,
  diameter,
  height,
  plot_size,
  .groups = NULL,
  plot_shape = "circular",
  dmin = 7.5,
  dmax = NULL,
  class_length = 5,
  include_lowest = TRUE,
  which_h0 = "assman",
  which_spacing = "hart"
)
```

## Arguments

- data:

  A tibble of inventory data

- diameter:

  Numeric vector of diameters or diameter classes

- height:

  Numeric vector of tree heights

- plot_size:

  The size of the plot. See
  [`silv_density_ntrees_ha()`](https://cidree.github.io/silviculture/reference/silv_density_ntrees_ha.md)

- .groups:

  A character vector with variables to group by (e.g. plot id, tree
  species, etc)

- plot_shape:

  The shape of the sampling plot. Either `circular` or `rectangular`

- dmin:

  The minimum inventory diameter in centimeters

- dmax:

  The maximum inventory diameter in centimeters. Values that are greater
  than `dmax` are included in the greatest class

- class_length:

  The length of the class in centimeters

- include_lowest:

  Logical. If TRUE (the default), the intervals are `[dim1, dim2)`. If
  FALSE, the intervals are `(dim1, dim2]`

  \[dim1, dim2)`. If FALSE, the intervals are `(dim1, dim2\]:
  R:dim1,%20dim2)%60.%20If%20FALSE,%20the%20intervals%20are%20%60(dim1,%20dim2

- which_h0:

  The method to calculate the dominant height. See
  [`silv_stand_dominant_height()`](https://cidree.github.io/silviculture/reference/silv_stand_dominant_height.md)

- which_spacing:

  A character with the name of the index (either `hart` or
  `hart-brecking`). See
  [`silv_density_hart()`](https://cidree.github.io/silviculture/reference/silv_density_hart.md)

## Value

an S7 `Inventory` list with 2 `tibbles`

## Details

The function calculates many inventory parameters and returns two
tibbles:

- **dclass_metrics**: metrics summarized by .groups and diametric
  classes

- **group_metrics**: metrics summarized by .groups

## Examples

``` r
silv_summary(
  data      = inventory_samples,
  diameter  = diameter,
  height    = height,
  plot_size = 10,
  .groups   = c("plot_id", "species")
 )
#> <silviculture::Inventory>
#>  @ dclass_metrics: tibble [57 × 9] (S3: tbl_df/tbl/data.frame)
#>  $ plot_id  : int [1:57] 7 7 7 7 7 7 7 8 8 8 ...
#>  $ species  : int [1:57] 27 27 27 27 27 27 27 28 28 81 ...
#>  $ dclass   : num [1:57] 50 55 35 45 60 25 120 55 60 10 ...
#>  $ height   : num [1:57] 18 17.6 16.5 14.6 19.1 ...
#>  $ ntrees   : int [1:57] 3 5 1 2 3 1 1 1 1 3 ...
#>  $ ntrees_ha: num [1:57] 95.5 159.2 31.8 63.7 95.5 ...
#>  $ h0       : num [1:57] 19.7 19.7 19.7 19.7 19.7 ...
#>  $ dg       : num [1:57] 57.9 57.9 57.9 57.9 57.9 ...
#>  $ g_ha     : num [1:57] 18.75 37.81 3.06 10.12 27 ...
#>  @ group_metrics : tibble [14 × 15] (S3: tbl_df/tbl/data.frame)
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
#>  $ h0       : num [1:14] 19.65 17.5 6.39 5.15 7.12 ...
#>  $ ntrees   : int [1:14] 16 2 8 7 5 6 4 10 5 19 ...
#>  $ ntrees_ha: num [1:14] 509.3 63.7 254.6 222.8 159.2 ...
#>  $ g_ha     : num [1:14] 134.31 16.56 5.25 4 2.5 ...
#>  $ spacing  : num [1:14] 22.6 71.6 98.1 130.2 111.3 ...
#>  @ groups        : chr [1:2] "plot_id" "species"
```
