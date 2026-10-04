# Calculate Tree Slenderness

Calculate Tree Slenderness

## Usage

``` r
silv_tree_slenderness(diameter, height, d_units = "cm", h_units = "m")
```

## Arguments

- diameter:

  A numeric vector of tree DBH.

- height:

  A numeric vector of tree height.

- d_units:

  Units of the diameter.

- h_units:

  Units of the height.

## Value

A numeric vector with the tree slenderness.

## Examples

``` r
silv_tree_slenderness(diameter = 20, height = 15)
#> [1] 75
```
