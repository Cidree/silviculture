# Calculate Tree Basal Area per Hectare

Calculates the tree basal area expanded to hectare.

## Usage

``` r
silv_tree_basal_area_ha(
  diameter = NULL,
  basal_area = NULL,
  expansion_factor,
  units = "cm"
)
```

## Arguments

- diameter:

  A numeric vector of tree DBH. Optional if `basal_area` is provided.

- basal_area:

  A numeric vector of tree basal area in m2. Optional if `diameter` is
  provided.

- expansion_factor:

  A numeric vector representing the tree expansion factor.

- units:

  The units of the diameter (one of `mm`, `cm`, `dm`, or `m`).

## Value

A numeric vector with the basal area per hectare (m2/ha).

## Examples

``` r
silv_tree_basal_area_ha(diameter = 20, expansion_factor = 127)
#> [1] 3.989823
```
