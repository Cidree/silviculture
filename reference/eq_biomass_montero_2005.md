# Biomass equations for Spanish species

Allometric equations adjusted for Spanish species

## Usage

``` r
eq_biomass_montero_2005(species, component = "stem", return_r2 = FALSE)
```

## Arguments

- species:

  A character string specifying the scientific name of the tree species.
  It can be a column name if all the species are included in this model.
  See Details for available species.

- component:

  A character string specifying the tree component for biomass
  calculation (e.g., "tree", "stem", "branches"). See Details.

- return_r2:

  A logical value. If TRUE, the function returns the root mean squared
  error (RMSE) of the selected model instead of the biomass value.

## Value

A S7 list of parameters

## Details

There are 35 species included in the model.

The tree components are divided into groups, and any of them can be
introduced in the component argument:

- **AGB**: all aboveground biomass components

- **BGB**: all belowground biomass compoponents

- **tree** or **all**: total tree biomass includying AGB and BGB

Then we have the second group of components, which are related to tree
groups:

- **stem**: includes the stem and bark

- **branches**: includes all branches

- **roots**: includes the roots (same as BGB)

Finally, we have the last level, which includes tree components (not all
of them are available for all species): stem, bark, thick branches
(\>7cm), medium branches (2-7cm), thin branches (0.5-2cm), leaves
(include needles), roots. In some species, there's "stem and thick
branches", instead of two groups.

Users can check the list of supported species and their corresponding
components in
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md).

## See also

[`silv_predict_biomass()`](https://cidree.github.io/silviculture/reference/silv_predict_biomass.md),
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md),
[`eq_biomass_dieguez_aranda_2009()`](https://cidree.github.io/silviculture/reference/eq_biomass_dieguez_aranda_2009.md)
[`eq_biomass_ruiz_peinado_2011()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2011.md),
[`eq_biomass_ruiz_peinado_2012()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2012.md),
[`eq_biomass_manrique_2017()`](https://cidree.github.io/silviculture/reference/eq_biomass_manrique_2017.md),
[`eq_biomass_menendez_2022()`](https://cidree.github.io/silviculture/reference/eq_biomass_menendez_2022.md),
[`eq_biomass_cudjoe_2024()`](https://cidree.github.io/silviculture/reference/eq_biomass_cudjoe_2024.md)

## Examples

``` r
## get model parameters for silv_predict_biomass
eq_biomass_montero_2005("Pinus pinaster", "AGB")
#> <silviculture::ModelBiomass>
#>  @ equation  : chr "montero-2005"
#>  @ species   : chr "Pinus pinaster"
#>  @ component : chr "AGB"
#>  @ expression:'data.frame':  5 obs. of  2 variables:
#>  .. $ expression: chr  "exp((0.191593 ^2) / 2) * exp(-3.43957) * (d ^ 2.56636)" "exp((0.324283 ^2) / 2) * exp(-23.0418) * (d ^ 6.52359)" "exp((0.744427 ^2) / 2) * exp(-6.66264) * (d ^ 2.63946)" "exp((0.527572 ^2) / 2) * exp(-4.66658) * (d ^ 2.38009)" ...
#>  .. $ species   : chr  "Pinus pinaster" "Pinus pinaster" "Pinus pinaster" "Pinus pinaster" ...
#>  @ url       : chr "https://gregoriomontero.wordpress.com/wp-content/uploads/2016/09/2005-01-monografc3ada-forestal-13-m-produccic3"| __truncated__
#>  @ obs       : chr "Diameter is assumed to in centimeters, and height is assumed to be in meters"
#>  @ params    :List of 3
#>  .. $ return_r2: logi FALSE
#>  .. $ comp     : chr [1:5] "stem" "thick branches" "medium branches" "small branches" ...
#>  .. $ r2       : num [1:5] 0.964 0.927 0.651 0.752 NA
```
