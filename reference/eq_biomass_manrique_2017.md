# Biomass equations two Quercus species

Allometric equations adjusted for *Quercus petraea* and *Quercus
pyrenaica* in Palencia, Spain

## Usage

``` r
eq_biomass_manrique_2017(
  species,
  component = "AGB",
  return_r2 = FALSE,
  return_rmse = FALSE
)
```

## Arguments

- species:

  A character string specifying the scientific name of the tree species.
  It can be a column name if all the species are included in this model.
  See Details for available species.

- component:

  A character string specifying the tree component for biomass
  calculation (e.g., "stem", "branches"). See Details.

- return_r2:

  A logical value. If TRUE, the function returns the root mean squared
  error (RMSE) of the selected model instead of the biomass value.

- return_rmse:

  A logical value. If TRUE, the function returns the root mean squared
  error (RMSE) of the selected model instead of the biomass value.

## Value

A S7 list of parameters

## Details

There are two species in this model: *Quercus petraea* and *Quercus
pyrenaica*

The tree components include:

- **stem**: includes stem and the thickest branches

- **medium branches**

- **thin branches**

- **AGB**: total biomass, results of summing the previous three
  components

## See also

[`silv_predict_biomass()`](https://cidree.github.io/silviculture/reference/silv_predict_biomass.md),
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md),
[`eq_biomass_montero_2005()`](https://cidree.github.io/silviculture/reference/eq_biomass_montero_2005.md),
[`eq_biomass_dieguez_aranda_2009()`](https://cidree.github.io/silviculture/reference/eq_biomass_dieguez_aranda_2009.md)
[`eq_biomass_ruiz_peinado_2011()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2011.md),
[`eq_biomass_ruiz_peinado_2012()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2012.md),
[`eq_biomass_menendez_2022()`](https://cidree.github.io/silviculture/reference/eq_biomass_menendez_2022.md),
[`eq_biomass_cudjoe_2024()`](https://cidree.github.io/silviculture/reference/eq_biomass_cudjoe_2024.md)

## Examples

``` r
## get model parameters for silv_predict_biomass
eq_biomass_manrique_2017("Quercus petraea", "AGB")
#> <silviculture::ModelBiomass>
#>  @ equation  : chr "manrique-2017"
#>  @ species   : chr "Quercus petraea"
#>  @ component : chr "AGB"
#>  @ expression:'data.frame':  3 obs. of  2 variables:
#>  .. $ expression: chr  "0.001333 * (d ** 2) * h" "0.006531 * (d ** 2) * h - 0.07298 * d * h" "0.023772 * (d ** 2) * h"
#>  .. $ species   : chr  "Quercus petraea" "Quercus petraea" "Quercus petraea"
#>  @ url       : chr "https://7cfe.congresoforestal.es/sites/default/files/comunicaciones/772.pdf"
#>  @ obs       : chr "Diameter is assumed to in centimeters, and height is assumed to be in meters"
#>  @ params    :List of 5
#>  .. $ return_r2  : logi FALSE
#>  .. $ return_rmse: logi FALSE
#>  .. $ comp       : chr [1:3] "stem and thick branches" "medium branches" "small branches"
#>  .. $ r2         : num [1:3] 0.868 0.873 0.99
#>  .. $ rmse       : num [1:3] 29197 120699 155704
```
