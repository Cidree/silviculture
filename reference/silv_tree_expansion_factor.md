# Calculate Tree Expansion Factor

Calculates the tree expansion factor according to the plot type (fixed
area or SNFI concentric plots).

## Usage

``` r
silv_tree_expansion_factor(
  type = c("fixed_area", "snfi"),
  plot_area = NULL,
  diameter = NULL,
  d_units = "cm",
  a_units = "m2"
)
```

## Arguments

- type:

  Plot type, either `"fixed_area"` or `"snfi"`.

- plot_area:

  Plot area (required for `"fixed_area"`).

- diameter:

  Tree DBH (required for `"snfi"`).

- d_units:

  Units of the diameter.

- a_units:

  Units of the plot area (one of `m2` or `ha`).

## Value

A numeric vector with the expansion factor.

## Examples

``` r
silv_tree_expansion_factor(type = "fixed_area", plot_area = 500)
#> [1] 20
silv_tree_expansion_factor(type = "snfi", diameter = 15)
#> [1] 31.83099
```
