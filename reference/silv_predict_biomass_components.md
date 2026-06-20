# Predict All Individual Biomass Components for Trees

Predicts all available individual biomass components (e.g., stem, bark,
branches, roots) for the given trees in a single call, returning a wide
data frame.

## Usage

``` r
silv_predict_biomass_components(
  species,
  diameter,
  height = NULL,
  model_fn,
  ntrees = NULL,
  rcd = NULL,
  bp = NULL,
  quiet = TRUE
)
```

## Arguments

- species:

  A character vector specifying the scientific names of the tree
  species.

- diameter:

  A numeric vector of tree diameters at breast height (in cm).

- height:

  An optional numeric vector of tree heights (in m). Defaults to `NULL`.

- model_fn:

  A function or character string. The constructer function of the model
  (e.g., `eq_biomass_ruiz_peinado_2011`) or the model ID string (e.g.,
  `"ruiz-peinado-2011"`).

- ntrees:

  An optional numeric vector indicating the number of trees in each
  class. Defaults to `NULL`.

- rcd:

  An optional numeric vector of root collar diameters (in cm).

- bp:

  An optional numeric vector of biomass packing values (in m³).

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.

## Value

A `data.frame` with the columns `species`, `diameter`, `height` (if
provided), and one additional column for each individual biomass
component available for the selected species and model.

## Examples

``` r
# Predict all components using Ruiz-Peinado 2011
silv_predict_biomass_components(
  species = "Pinus pinaster",
  diameter = 25,
  height = 15,
  model_fn = eq_biomass_ruiz_peinado_2011
)
#>          species diameter height     stem thick and medium branches
#> 1 Pinus pinaster       25     15 134.1258                   9.37248
#>   small branches and leaves    roots
#> 1                   22.5845 36.91533
```
