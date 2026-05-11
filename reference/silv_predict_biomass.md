# Calculate Tree Biomass

Computes the biomass of a tree species using species-specific allometric
equations (in kg). Currently, only equations for Spain are available.

## Usage

``` r
silv_predict_biomass(
  diameter = NULL,
  height = NULL,
  model,
  ntrees = NULL,
  quiet = FALSE
)
```

## Arguments

- diameter:

  A numeric vector of tree diameters (in cm).

- height:

  A numeric vector of tree heights (in m).

- model:

  A function. A function with the structure `eq_biomass_*()` with
  additional arguments depending on the model used.

- ntrees:

  An optional numeric value indicating the number of trees in this
  diameter-height class. Defaults to 1 if `NULL`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.

## Value

A numeric vector

## Details

The function estimates biomass using validated allometric models
available in the dataset
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md).
The available models include:

- **[`eq_biomass_ruiz_peinado_2011()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2011.md)**:
  Developed for softwood species in Spain.

- **[`eq_biomass_ruiz_peinado_2012()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2012.md)**:
  Developed for hardwood species in Spain.

Users can check the list of supported species and their corresponding
components in
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md).

If you would like to suggest additional models, please open a new issue
on GitHub.

## See also

[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md),
[`eq_biomass_montero_2005()`](https://cidree.github.io/silviculture/reference/eq_biomass_montero_2005.md),
[`eq_biomass_dieguez_aranda_2009()`](https://cidree.github.io/silviculture/reference/eq_biomass_dieguez_aranda_2009.md),
[`eq_biomass_ruiz_peinado_2011()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2011.md),
[`eq_biomass_ruiz_peinado_2012()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2012.md),
[`eq_biomass_manrique_2017()`](https://cidree.github.io/silviculture/reference/eq_biomass_manrique_2017.md),
[`eq_biomass_menendez_2022()`](https://cidree.github.io/silviculture/reference/eq_biomass_menendez_2022.md),
[`eq_biomass_cudjoe_2024()`](https://cidree.github.io/silviculture/reference/eq_biomass_cudjoe_2024.md)

## Examples

``` r
# Calculate biomass for a single tree
silv_predict_biomass(
  diameter = 45,
  height   = 22,
  model    = eq_biomass_ruiz_peinado_2011("Pinus pinaster")
)
#> ! Cite this model using <https://doi.org/10.5424/fs/2011201-11643>
#> ℹ Diameter is assumed to in centimeters, and height is assumed to be in meters
#> [1] 589.1237
```
