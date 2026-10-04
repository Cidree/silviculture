# Calculates the Stand Density Index

The Stand Density Index (SDI) is the relationship between the average
tree size and density of trees per hectare.

## Usage

``` r
silv_density_sdi(ntrees, dg, beta = 1.605)
```

## Arguments

- ntrees:

  Numeric vector with number of trees of the diameter class per hectare.
  If `ntrees = NULL`, the function will assume that each diameter
  corresponds to only one tree

- dg:

  Numeric vector of quadratic mean diameters

- beta:

  The Stand Density Index exponent (default is `1.605`).

## Value

A numeric vector representing the absolute SDI.

## Details

The SDI has different interpretations depending on the species,
location, and also the management type (even-aged, uneven-aged...). The
value of maximum SDI must be determined from the literature and used
carefully. The `beta` exponent allows adjustments for different species
or mixed stands.

## References

Reineke, L. H. (1933). Perfecting a stand-density index for even-aged
forests. Journal of Agricultural Research, 46(7), 627-638. URL:
https://research.fs.usda.gov/download/treesearch/60134.pdf

## Examples

``` r
## calculate SDI for a Pinus sylvestris stand (beta = 1.605)
silv_density_sdi(ntrees = 800, dg = 23.4)
#> [1] 701.3315

## calculate SDI with custom beta
silv_density_sdi(ntrees = 800, dg = 23.4, beta = 1.7)
#> [1] 695.8884
```
