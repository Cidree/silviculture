# Biomass equations for young Spanish plantations

Allometric equations for young (\<30) plantations of 18 Spanish species
including broadleaf and conifer species. Only aboveground biomass.

## Usage

``` r
eq_biomass_menendez_2022(species, return_r2 = FALSE, return_rmse = FALSE)
```

## Arguments

- species:

  A character string specifying the scientific name of the tree species.
  It can be a column name if all the species are included in this model.
  See Details for available species.

- return_r2:

  A logical value. If TRUE, the function returns the root mean squared
  error (RMSE) of the selected model instead of the biomass value.

- return_rmse:

  A logical value. If TRUE, the function returns the root mean squared
  error (RMSE) of the selected model instead of the biomass value.

## Value

A S7 list of parameters

## Details

There are 15 species in this model, including generic equations for
*Conifers*, *Deciduous broadleaves*, and *Evergreen broadleaves*.

All the models measure only aboveground biomass.

## See also

[`silv_predict_biomass()`](https://cidree.github.io/silviculture/reference/silv_predict_biomass.md),
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md),
[`eq_biomass_montero_2005()`](https://cidree.github.io/silviculture/reference/eq_biomass_montero_2005.md),
[`eq_biomass_dieguez_aranda_2009()`](https://cidree.github.io/silviculture/reference/eq_biomass_dieguez_aranda_2009.md)
[`eq_biomass_ruiz_peinado_2011()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2011.md),
[`eq_biomass_ruiz_peinado_2012()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2012.md),
[`eq_biomass_manrique_2017()`](https://cidree.github.io/silviculture/reference/eq_biomass_manrique_2017.md),
[`eq_biomass_cudjoe_2024()`](https://cidree.github.io/silviculture/reference/eq_biomass_cudjoe_2024.md)

## Examples

``` r
## get model parameters for silv_predict_biomass
eq_biomass_menendez_2022("Fagus sylvatica")
#> <silviculture::ModelBiomass>
#>  @ equation  : chr "menendez-2022"
#>  @ species   : chr "Fagus sylvatica"
#>  @ component : chr "AGB"
#>  @ expression:'data.frame':  1 obs. of  2 variables:
#>  .. $ expression: chr "0.0574 * ((rcd ** 2) * h) ** 0.8930"
#>  .. $ species   : chr "Fagus sylvatica"
#>  @ url       : chr "https://doi.org/10.1016/j.biombioe.2022.106453"
#>  @ obs       : chr "AGB is the aboveground dry biomass or aerial biomass (kg), RCD is the root-collar-diameter (cm), h is the total"| __truncated__
#>  @ params    :List of 5
#>  .. $ return_r2  : logi FALSE
#>  .. $ return_rmse: logi FALSE
#>  .. $ comp       : chr "agb"
#>  .. $ r2         : num 0.97
#>  .. $ rmse       : num 3.5
```
