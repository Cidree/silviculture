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
