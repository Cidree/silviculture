# Predict carbon content from biomass

`silv_predict_carbon()` calculates the carbon content of a tree
component based on its predicted biomass. It uses species- and
component-specific carbon percentages from the package's internal
`carbon_models` database.

`silv_predict_carbon_auto()` is a vectorized function that automatically
selects the best available carbon model for each row based on a provided
priority list. If the exact species is not found, it attempts a
genus-level fallback (e.g., "Pinus spp.").

## Usage

``` r
silv_predict_carbon(
  biomass,
  species,
  component,
  model = "montero-2005",
  quiet = FALSE
)

silv_predict_carbon_auto(
  biomass,
  species,
  component,
  priority = c("montero-2005", "dieguez-aranda-2009"),
  quiet = FALSE
)
```

## Arguments

- biomass:

  A numeric vector of predicted biomass (in kg).

- species:

  A character vector of tree species (e.g., `"Pinus pinaster"`).

- component:

  A character string or vector specifying the tree component (e.g.,
  `"stem"`, `"roots"`, `"tree"`). Must match the `tree_component` column
  in the
  [carbon_models](https://cidree.github.io/silviculture/reference/carbon_models.md)
  dataset. Note that some models do not provide carbon percentages for
  aggregate groups like `"AGB"`.

- model:

  A character string specifying the carbon model to use (e.g.,
  `"montero-2005"`).

- quiet:

  Logical. If `FALSE`, informs the user about genus fallbacks or missing
  species.

- priority:

  A character vector specifying the order of preference for carbon
  models. Default is `c("montero-2005", "dieguez-aranda-2009")`.

## Value

For `silv_predict_carbon()`: A numeric vector of predicted carbon (in
kg). Returns `NA` if the species/component combination is not supported
by the model.

For `silv_predict_carbon_auto()`: A `data.frame` with two columns:

- `carbon`: The predicted carbon (in kg).

- `carbon_model`: The model and species level used (e.g.,
  `"montero-2005"` or `"montero-2005 (genus fallback)"`).

## Examples

``` r
# 1. Vector-based calculation: calculate stem carbon for Pinus pinaster
biomass_vector <- c(120.5, 230.1, 85.4)
species_vector <- c("Pinus pinaster", "Pinus pinaster", "Pinus pinaster")

carbon_vector <- silv_predict_carbon(
  biomass = biomass_vector,
  species = species_vector,
  component = "stem",
  model = "montero-2005"
)
print(carbon_vector)
#> [1]  61.5755 117.5811  43.6394

# 2. Vector-based calculation with row-wise auto-selection & genus fallback
carbon_df <- silv_predict_carbon_auto(
  biomass = c(120.5, 95.0),
  species = c("Pinus pinaster", "Pinus radiata"), # Pinus radiata triggers genus fallback
  component = c("stem", "roots")
)
print(carbon_df)
#>    carbon carbon_model
#> 1 61.5755 montero-2005
#> 2 47.2150 montero-2005

# 3. Dataset-based tutorial: apply to a forest inventory data frame
inventory <- data.frame(
  tree_id   = 1:3,
  species   = c("Pinus pinaster", "Pinus sylvestris", "Quercus robur"),
  biomass_kg = c(150.0, 180.5, 220.0),
  component = c("stem", "roots", "stem")
)

# Apply row-wise auto-selection to the entire dataset
carbon_results <- silv_predict_carbon_auto(
  biomass   = inventory$biomass_kg,
  species   = inventory$species,
  component = inventory$component
)

# Combine and display results
inventory_with_carbon <- cbind(inventory, carbon_results)
print(inventory_with_carbon)
#>   tree_id          species biomass_kg component   carbon carbon_model
#> 1       1   Pinus pinaster      150.0      stem  76.6500 montero-2005
#> 2       2 Pinus sylvestris      180.5     roots  91.8745 montero-2005
#> 3       3    Quercus robur      220.0      stem 106.4800 montero-2005
```
