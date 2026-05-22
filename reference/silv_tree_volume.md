# Calculate Tree Volume

This function calculates the volume of a tree or logs using different
formulas: Pressler, Huber, Smalian, and Newton. The appropriate diameter
and height parameters must be provided depending on the selected
formula.

## Usage

``` r
silv_tree_volume(
  diameter_base = NULL,
  diameter_top = NULL,
  diameter_center = NULL,
  diameter = NULL,
  height = NULL,
  formula = "pressler",
  ntrees = NULL
)
```

## Arguments

- diameter_base:

  A numeric vector. The diameter at the base of the tree (required for
  Pressler, Smalian, and Newton formulas).

- diameter_top:

  A numeric vector. The diameter at the top of the tree (required for
  Smalian and Newton formulas).

- diameter_center:

  A numeric vector. The diameter at the center of the tree (required for
  Huber and Newton formulas).

- diameter:

  A numeric vector. The diameter at breast height (used in Pressler
  formula if provided instead of `diameter_base`).

- height:

  A numeric vector. The tree or log height (required for all formulas).

- formula:

  Character. The volume formula to use. Options: `"pressler"`,
  `"huber"`, `"smalian"`, `"newton"`. Default is `"pressler"`.

- ntrees:

  A numeric vector with number of trees of the same dimensions. Default
  is 1.

## Value

A numeric value representing the tree volume.

## Examples

``` r
silv_tree_volume(diameter_base = 30, height = 20, formula = "pressler")
#> ! When using Pressler formula, height is assumed to be the Pressler directrix point (i.e., the height at which the stem diameter is half the base diameter).
#> [1] 0.9424778
silv_tree_volume(diameter_center = 25, height = 15, formula = "huber")
#> [1] 0.7363108
silv_tree_volume(diameter_base = 30, diameter_top = 20, height = 20, formula = "smalian")
#> [1] 1.021018
```
