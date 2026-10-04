# Calculates the Maximum Stand Density Index (SDImax)

The Maximum Stand Density Index (SDImax) represents the maximum stand
carrying capacity, calculated using coefficients from Rodríguez de Prado
(2020) by default.

## Usage

``` r
silv_density_sdimax(
  species,
  model = "rodriguez-prado-2020",
  climatic_model = NULL,
  clim_value = NULL
)
```

## Arguments

- species:

  Character vector. Scientific names of the tree species.

- model:

  Character. The source article or model database (default is
  `"rodriguez-prado-2020"`).

- climatic_model:

  Character. The specific climate-dependent model name (e.g. `"P1"`,
  `"MXT3"`). Required if `clim_value` is provided, and must not be
  `"basic"`.

- clim_value:

  Numeric vector. Values of the climatic variable corresponding to the
  selected climate model. If `NULL` (default), the reference model
  (`"basic"`) is calculated.

## Value

A numeric vector representing the SDImax for each species.

## Details

If `clim_value` is `NULL`, the function computes the reference SDImax
(SDImaxREF) based on the "basic" model parameters: \$\$SDImaxREF =
exp(a0 + b0 \* log(25.4))\$\$ If `clim_value` is provided, a
climate-dependent model must be specified in `climatic_model`, and the
climate-dependent SDImax is calculated as: \$\$SDImax(Clim) = exp((a0 +
a1 \* log(clim_value)) + (b0 + b1 \* clim_value) \* log(25.4))\$\$

## References

Rodríguez-de-Prado, M., et al. (2020). Potential climatic influence on
maximum stand carrying capacity for 15 Mediterranean coniferous and
broadleaf species. Forest Ecology and Management, 458, 117824.

## Examples

``` r
## Calculate reference SDImax for Pinus sylvestris
silv_density_sdimax("Pinus sylvestris")
#> [1] 1114.795

## Calculate climate-dependent SDImax for Pinus canariensis using model P1
silv_density_sdimax("Pinus canariensis", climatic_model = "P1", clim_value = 400)
#> [1] 103610.2
```
