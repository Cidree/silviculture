# Automatically Predict Biomass Using the Best Available Model

Evaluates vectors of tree species, diameters, and heights, and
automatically selects the best available allometric model based on a
specified priority. If tree height is not provided, is NA, or is 0, the
function automatically falls back to the
[`eq_biomass_montero_2005`](https://cidree.github.io/silviculture/reference/eq_biomass_montero_2005.md)
model.

## Usage

``` r
silv_predict_biomass_auto(
  species,
  diameter,
  height = NULL,
  component = "tree",
  ntrees = NULL,
  rcd = NULL,
  bp = NULL,
  priority = c("ruiz-peinado-2011", "ruiz-peinado-2012", "montero-2005",
    "dieguez-aranda-2009", "manrique-2017", "menendez-2022", "cudjoe-2024"),
  quiet = FALSE
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

- component:

  A character string specifying the tree component for biomass
  calculation (e.g., "tree", "stem", "branches"). Defaults to `"tree"`.

- ntrees:

  An optional numeric vector indicating the number of trees in each
  class. Defaults to `NULL` (equivalent to 1 tree per entry).

- rcd:

  An optional numeric vector of root collar diameters (in cm). Required
  for young plantation equations
  ([`eq_biomass_menendez_2022`](https://cidree.github.io/silviculture/reference/eq_biomass_menendez_2022.md)).

- bp:

  An optional numeric vector of biomass packing values (in m³). Required
  for some species in young plantation equations.

- priority:

  A character vector specifying the priority order for model selection.
  Defaults to
  `c("ruiz-peinado-2011", "ruiz-peinado-2012", "montero-2005", "dieguez-aranda-2009", "manrique-2017", "menendez-2022", "cudjoe-2024")`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages and
  warnings.

## Value

A `data.frame` containing two columns:

- `biomass`: Numeric vector of predicted biomass values (in kg).

- `biomass_model`: Character vector specifying the model ID used.

## Examples

``` r
# 1. Vector-based calculation: automatic model selection for mixed species
species_vec <- c("Pinus pinaster", "Quercus petraea")
d_vec <- c(20, 25)
h_vec <- c(12, 14)
auto_results <- silv_predict_biomass_auto(species_vec, d_vec, h_vec)
#> Warning: No compatible model was found in priority for species: "Quercus petraea" with
#> component "tree".
#> ! Cite this model using <https://doi.org/10.5424/fs/2011201-11643>
#> ℹ Diameter is assumed to in centimeters, and height is assumed to be in meters
print(auto_results)
#>    biomass     biomass_model
#> 1 110.7419 ruiz-peinado-2011
#> 2       NA              <NA>

# 2. Vector-based calculation: fallback to Montero 2005 when height is missing
fallback_results <- silv_predict_biomass_auto(species_vec, d_vec, height = NULL)
#> Warning: No compatible model was found in priority for species: "Quercus petraea" with
#> component "tree".
#> ! Cite this model using <https://gregoriomontero.wordpress.com/wp-content/uploads/2016/09/2005-01-monografc3ada-forestal-13-m-produccic3b3n-de-biomasa-y-fijacic3b3n-de-co2-por-los-bosques-espac3b1oles.pdf>
#> ℹ Diameter is assumed to in centimeters, and height is assumed to be in meters
print(fallback_results)
#>    biomass biomass_model
#> 1 89.13531  montero-2005
#> 2       NA          <NA>

# 3. Dataset-based tutorial: apply to a mixed-species forest inventory data frame
inventory <- data.frame(
  tree_id  = 1:3,
  species  = c("Pinus pinaster", "Quercus petraea", "Pinus sylvestris"),
  dbh_cm   = c(22.5, 18.0, 31.2),
  height_m = c(15.0, 11.5, 18.0)
)

# Run auto-selection and bind the results directly
biomass_data <- silv_predict_biomass_auto(
  species  = inventory$species,
  diameter = inventory$dbh_cm,
  height   = inventory$height_m
)
#> Warning: No compatible model was found in priority for species: "Quercus petraea" with
#> component "tree".
#> ! Cite this model using <https://doi.org/10.5424/fs/2011201-11643>
#> ℹ Diameter is assumed to in centimeters, and height is assumed to be in meters

inventory_with_biomass <- cbind(inventory, biomass_data)
print(inventory_with_biomass)
#>   tree_id          species dbh_cm height_m  biomass     biomass_model
#> 1       1   Pinus pinaster   22.5     15.0 159.2250 ruiz-peinado-2011
#> 2       2  Quercus petraea   18.0     11.5       NA              <NA>
#> 3       3 Pinus sylvestris   31.2     18.0 460.3449 ruiz-peinado-2011
```
