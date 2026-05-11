# Calculate Tree Biomass

**\[deprecated\]**

Computes the biomass of a tree species using species-specific allometric
equations (in kg).

## Usage

``` r
silv_biomass(
  diameter = NULL,
  height = NULL,
  ntrees = NULL,
  species = NULL,
  component = "stem",
  model = "ruiz-peinado-2012",
  return_rmse = FALSE,
  quiet = FALSE
)
```

## Arguments

- diameter:

  A numeric vector of tree diameters (in cm).

- height:

  A numeric vector of tree heights (in m).

- ntrees:

  An optional numeric value indicating the number of trees in this
  diameter-height class. Defaults to 1 if NULL.

- species:

  A character string specifying the scientific name of the tree species.
  See Details for available species.

- component:

  A character string specifying the tree component for biomass
  calculation (e.g., "tree", "stem", "branches"). See Details.

- model:

  A character string indicating the ID of the publication in which the
  model was developed. Currently supported models: "ruiz-peinado-2012"
  (hardwood species in Spain) and "ruiz-peinado-2011" (softwood species
  in Spain). See Details.

- return_rmse:

  A logical value. If TRUE, the function returns the root mean squared
  error (RMSE) of the selected model instead of the biomass value.

- quiet:

  A logical value. If TRUE, suppresses any informational messages.

## Value

A numeric vector of biomass values (in kg). If `return_rmse = TRUE`,
returns the RMSE instead.

## Details

The function estimates biomass using validated allometric models
available in the dataset
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md).
The available models include:

- **ruiz-peinado-2011**: Developed for softwood species in Spain.

- **ruiz-peinado-2012**: Developed for hardwood species in Spain.

Users can check the list of supported species and their corresponding
components in
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md).

If you would like to suggest additional models, please open a new issue
on GitHub.

## Examples

``` r
# Calculate biomass for a single tree
silv_biomass(
  diameter = 45,
  height   = 22,
  species  = "Pinus pinaster",
  model    = "ruiz-peinado-2011"
)
#> Warning: `silv_biomass()` was deprecated in silviculture 0.2.0.
#> ℹ Function `silv_biomass() is deprecated in favour of `silv_predict_biomass()`,
#>   and it will be removed in the next release.
#> ! Cite this model using <https://doi.org/10.5424/fs/2011201-11643>
#> [1] 589.1237
```
