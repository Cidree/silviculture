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

A S7 list of parameters

## Details

There are three species options in this model:

- ***Quercus petraea***

- ***Pinus sylvestris***

- **Mixed**: stands with *Quercus petraea* and *Pinus sylvestris*

The tree components include some AGB components:

- **leaves**: only for *P. sylvestris*

- **stem**: for all species

- **medium branches and small brances**: for all species

- **thick branches**: for all species

- **AGB**: total biomass, results of summing the previous components

## See also

[`silv_predict_biomass()`](https://cidree.github.io/silviculture/reference/silv_predict_biomass.md),
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md),
[`eq_biomass_montero_2005()`](https://cidree.github.io/silviculture/reference/eq_biomass_montero_2005.md),
[`eq_biomass_dieguez_aranda_2009()`](https://cidree.github.io/silviculture/reference/eq_biomass_dieguez_aranda_2009.md)
[`eq_biomass_ruiz_peinado_2011()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2011.md),
[`eq_biomass_ruiz_peinado_2012()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2012.md),
[`eq_biomass_manrique_2017()`](https://cidree.github.io/silviculture/reference/eq_biomass_manrique_2017.md),
[`eq_biomass_menendez_2022()`](https://cidree.github.io/silviculture/reference/eq_biomass_menendez_2022.md)

## Examples

``` r
## get model parameters for silv_predict_biomass
eq_biomass_cudjoe_2024("mixed", "AGB")
#> <silviculture::ModelBiomass>
#>  @ equation  : chr "cudjoe-2017"
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
