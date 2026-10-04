# Calculates the Site Factor (SF)

The site factor is a variable used in growth equations based on the
dominant height and dominant dbh.

## Usage

``` r
silv_site_factor(species, d0, h0)
```

## Arguments

- species:

  Character vector. Scientific names of the tree species.

- d0:

  Numeric vector. Dominant diameter.

- h0:

  Numeric vector. Dominant height.

## Value

A numeric vector representing the Site Factor. `NA` for unsupported
species.

## References

Aguirre, A., et al. (2022).

## Examples

``` r
silv_site_factor(species = "Pinus sylvestris", d0 = 25, h0 = 15)
#> [1] 17.57314
```
