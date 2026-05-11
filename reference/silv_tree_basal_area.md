# Calculates Basal Area

Calculates Basal Area in square meters.

## Usage

``` r
silv_tree_basal_area(diameter, units = "cm")
```

## Arguments

- diameter:

  Numeric vector of diameters or diameter classes

- units:

  The units of the diameter (one of `mm`, `cm`, `dm`, or `m`)

## Value

A numeric vector

## Details

The function uses the next formula:

\\g = \frac{\pi}{40000} \cdot D^2\\

where g is the basal area in \\m^2\\ of one tree, and D is the diameter
in `cm`.

If you want to calculate the basal area for a group of trees (e.g. per
hectares), please use
[`silv_stand_basal_area()`](https://cidree.github.io/silviculture/reference/silv_stand_basal_area.md)

## See also

[`silv_stand_basal_area()`](https://cidree.github.io/silviculture/reference/silv_stand_basal_area.md)

## Examples

``` r
## calculate individual basal area
silv_tree_basal_area(c(23, 11, 43.5, 94))
#> [1] 0.041547563 0.009503318 0.148616967 0.693977817
```
