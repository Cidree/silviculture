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

A `data.frame` with the columns `species`, `diameter` (cm), `height` (m,
if provided), and one additional numeric column (in kg) for each
individual biomass component available for the selected species and
model.

## Examples

``` r
# 1. Vector-based calculation: predict all components for Pinus pinaster
comp_results <- silv_predict_biomass_components(
  species = c("Pinus pinaster", "Pinus pinaster"),
  diameter = c(20, 25),
  height = c(12, 15),
  model_fn = eq_biomass_ruiz_peinado_2011
)
print(comp_results)
#>          species diameter height     stem thick and medium branches
#> 1 Pinus pinaster       20     12  72.8882                  4.650078
#> 2 Pinus pinaster       25     15 134.1258                  9.372480
#>   small branches and leaves    roots
#> 1                  13.45796 19.74563
#> 2                  22.58450 36.91533

# 2. Dataset-based tutorial: apply to a forest inventory data frame
inventory <- data.frame(
  tree_id  = 1:3,
  species  = c("Pinus pinaster", "Pinus pinaster", "Pinus pinaster"),
  dbh_cm   = c(18.5, 22.1, 29.4),
  height_m = c(14.0, 16.5, 19.0)
)

# Predict components for the entire dataset
comp_df <- silv_predict_biomass_components(
  species  = inventory$species,
  diameter = inventory$dbh_cm,
  height   = inventory$height_m,
  model_fn = "ruiz-peinado-2011"
)

# Combine and display results (excluding repeated identifier columns)
inventory_with_components <- cbind(
  inventory, 
  comp_df[, -(1:3)]
)
print(inventory_with_components)
#>   tree_id        species dbh_cm height_m     stem thick and medium branches
#> 1       1 Pinus pinaster   18.5     14.0  67.9861                  3.640084
#> 2       2 Pinus pinaster   22.1     16.5 109.6073                  6.362988
#> 3       3 Pinus pinaster   29.4     19.0 218.7102                 15.595667
#>   small branches and leaves    roots
#> 1                  11.23125 15.86838
#> 2                  16.96601 26.12516
#> 3                  32.89694 58.16060
```
