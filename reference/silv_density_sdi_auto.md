# Predict Stand Density Index automatically

`silv_density_sdi_auto()` is a vectorized function that automatically
selects the best available Stand Density Index exponent (`beta`) for
each row based on a provided species, country, and region from the
internal `sdi_coefficients` database.

If an exact species, country, and region match is not found, the
function falls back to a country-wide species model (`region = "all"`),
then searches other countries, then falls back to a genus-level fallback
(e.g., "Pinus spp."), and finally to the default SDI exponent
(`beta = -1.605` from Reineke 1933).

## Usage

``` r
silv_density_sdi_auto(
  ntrees,
  dg,
  species,
  country = NULL,
  region = NULL,
  quiet = FALSE
)
```

## Arguments

- ntrees:

  Numeric vector with number of trees of the diameter class per hectare.
  If `ntrees = NULL`, the function will assume that each diameter
  corresponds to only one tree

- dg:

  Numeric vector of quadratic mean diameters

- species:

  A character string or vector of tree species (e.g.,
  `"Pinus sylvestris"`).

- country:

  A character string or vector of the country (e.g., `"Spain"`).
  Defaults to `NULL` (no country specified).

- region:

  A character string or vector of the region (e.g.,
  `"Castilla y León"`). Defaults to `NULL` (no region specified).

- quiet:

  Logical. If `FALSE`, informs the user about fallbacks to genus or
  default models.

## Value

A `data.frame` with three columns:

- `sdi`: The computed absolute Stand Density Index.

- `beta`: The beta exponent used for the calculation.

- `sdi_model`: The model used (e.g.,
  `"del-rio-2006 (Spain, Castilla y León)"`, `"reineke-1933 (-1.605)"`,
  etc.).

## Examples

``` r
# Calculate SDI with automatic selection
silv_density_sdi_auto(
  ntrees = 800,
  dg = 23.4,
  species = "Pinus sylvestris",
  region = "Castilla y León"
)
#> ℹ Exact country  not found for "Pinus sylvestris". Using fallback country "Spain" from "del-rio-2006".
#>        sdi  beta                             sdi_model
#> 1 693.0407 -1.75 del-rio-2006 (Spain, Castilla y León)

# Fallback to default
silv_density_sdi_auto(
  ntrees = 800,
  dg = 23.4,
  species = "Unknown species"
)
#> ℹ Exact model for "Unknown species" not found. Using default beta -1.605.
#>        sdi   beta             sdi_model
#> 1 701.3315 -1.605 reineke-1933 (-1.605)
```
