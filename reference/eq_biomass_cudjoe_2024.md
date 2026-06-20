# Biomass equations for 2 species in Castille and León (Spain)

Allometric equations adjusted for *Quercus petraea*, and *Pinus
sylvestris* in Castille and León (Spain)

## Usage

``` r
eq_biomass_cudjoe_2024(species, component = "AGB", return_rmse = FALSE)
```

## Arguments

- species:

  A character string specifying the scientific name of the tree species.
  It can be a column name if all the species are included in this model.
  See Details for available species.

- component:

  A character string specifying the tree component for biomass
  calculation (e.g., "stem", "branches"). See Details.

- return_rmse:

  A logical value. If TRUE, the function returns the root mean squared
  error (RMSE) of the selected model instead of the biomass value.

## Value

A ModelBiomass object containing the configured model parameters and
expressions.

## Details

**Supported species (3):**

- *Pinus sylvestris*

- *Quercus petraea*

- `"mixed"` — mixed stand of *Pinus sylvestris* × *Quercus petraea*

**Available components:**

Aboveground group (summed automatically):

- `"AGB"` — total aboveground biomass (sum of all components below)

Individual tree components (species availability varies):

- `"stem"` — stem wood (all species)

- `"thick branches"` — branches \> 7 cm (all species)

- `"medium branches and small branches"` — branches \< 7 cm (all
  species)

- `"leaves"` — foliage/needles (*Pinus sylvestris* only)

Note that no belowground biomass (BGB / roots) or total-tree equations
are available in the source paper for this model.

Users can check all available species and components in the
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md)
dataset provided by the library.

## See also

[`silv_predict_biomass()`](https://cidree.github.io/silviculture/reference/silv_predict_biomass.md),
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md),
[`eq_biomass_montero_2005()`](https://cidree.github.io/silviculture/reference/eq_biomass_montero_2005.md),
[`eq_biomass_dieguez_aranda_2009()`](https://cidree.github.io/silviculture/reference/eq_biomass_dieguez_aranda_2009.md),
[`eq_biomass_ruiz_peinado_2011()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2011.md),
[`eq_biomass_ruiz_peinado_2012()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2012.md),
[`eq_biomass_manrique_2017()`](https://cidree.github.io/silviculture/reference/eq_biomass_manrique_2017.md),
[`eq_biomass_menendez_2022()`](https://cidree.github.io/silviculture/reference/eq_biomass_menendez_2022.md)

## Examples

``` r
## Aboveground biomass for a mixed stand
eq_biomass_cudjoe_2024("mixed", "AGB")
#> <silviculture::ModelBiomass>
#>  @ equation  : chr "cudjoe-2024"
#>  @ species   : chr "Pinus sylvestris x Quercus petraea"
#>  @ component : chr "AGB"
#>  @ expression:'data.frame':  3 obs. of  2 variables:
#>  .. $ expression: chr  "0.1714*(-1) + 0.2388 * h" "0.7843 + 0.0952 * h" "1.8675*(-1) + 0.1974 * h"
#>  .. $ species   : chr  "Pinus sylvestris x Quercus petraea" "Pinus sylvestris x Quercus petraea" "Pinus sylvestris x Quercus petraea"
#>  @ url       : chr "https://doi.org/10.1016/j.scitotenv.2024.176061"
#>  @ obs       : chr "Diameter is assumed to in centimeters, and height is assumed to be in meters"
#>  @ params    :List of 4
#>  .. $ return_rmse: logi FALSE
#>  .. $ comp       : chr [1:3] "stem" "thick branches" "medium branches and small branches"
#>  .. $ r2         : num [1:3] NA NA NA
#>  .. $ rmse       : num [1:3] 0 0 0.1
```
