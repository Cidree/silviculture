# Biomass equations for Spanish hardwood species

Allometric equations adjusted for Spanish hardwood species

## Usage

``` r
eq_biomass_ruiz_peinado_2012(species, component = "stem", return_rmse = FALSE)
```

## Arguments

- species:

  A character string specifying the scientific name of the tree species.
  It can be a column name if all the species are included in this model.
  See Details for available species.

- component:

  A character string specifying the tree component for biomass
  calculation (e.g., "tree", "stem", "branches"). See Details.

- return_rmse:

  A logical value. If TRUE, the function returns the root mean squared
  error (RMSE) of the selected model instead of the biomass value.

## Value

A S7 list of parameters

## Details

Users can check the list of supported species and their corresponding
components in
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md).

## See also

[`silv_predict_biomass()`](https://cidree.github.io/silviculture/reference/silv_predict_biomass.md),
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md),
[`eq_biomass_montero_2005()`](https://cidree.github.io/silviculture/reference/eq_biomass_montero_2005.md),
[`eq_biomass_dieguez_aranda_2009()`](https://cidree.github.io/silviculture/reference/eq_biomass_dieguez_aranda_2009.md)
[`eq_biomass_ruiz_peinado_2011()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2011.md),
[`eq_biomass_manrique_2017()`](https://cidree.github.io/silviculture/reference/eq_biomass_manrique_2017.md),
[`eq_biomass_menendez_2022()`](https://cidree.github.io/silviculture/reference/eq_biomass_menendez_2022.md),
[`eq_biomass_cudjoe_2024()`](https://cidree.github.io/silviculture/reference/eq_biomass_cudjoe_2024.md)

## Examples

``` r
## get model parameters for silv_predict_biomass
eq_biomass_ruiz_peinado_2012("Quercus suber")
#> <silviculture::ModelBiomass>
#>  @ equation  : chr "ruiz-peinado-2012"
#>  @ species   : chr "Quercus suber"
#>  @ component : chr "stem"
#>  @ expression:'data.frame':  1 obs. of  2 variables:
#>  .. $ expression: chr "0.00525 * d^2 * h + 0.278 * d * h"
#>  .. $ species   : chr "Quercus suber"
#>  @ url       : chr "http://dx.doi.org/10.5424/fs/2112211-02193"
#>  @ obs       : chr "Diameter is assumed to in centimeters, and height is assumed to be in meters"
#>  @ params    :List of 3
#>  .. $ return_rmse: logi FALSE
#>  .. $ comp       : chr "stem"
#>  .. $ rmse       : num 66.9
```
