# Calculate Relative and Absolute Tree Coordinates

Calculate Relative and Absolute Tree Coordinates

## Usage

``` r
silv_tree_coordinates(
  distance,
  bearing,
  x_center = NULL,
  y_center = NULL,
  dist_units = "m",
  bearing_units = "grad"
)
```

## Arguments

- distance:

  Distance from plot center.

- bearing:

  Bearing from plot center.

- x_center:

  Plot center X coordinate (optional).

- y_center:

  Plot center Y coordinate (optional).

- dist_units:

  Units of the distance (one of `mm`, `cm`, `dm`, `m`).

- bearing_units:

  Units of the bearing (`degree`, `grad`, `radian`).

## Value

A data frame with relative coordinates (`x_rel`, `y_rel`) and, if plot
centers are provided, absolute coordinates (`x_abs`, `y_abs`).

## Examples

``` r
silv_tree_coordinates(distance = 10, bearing = 100)
#> # A tibble: 1 × 2
#>      x_rel y_rel
#>      <dbl> <dbl>
#> 1 6.12e-16    10
```
