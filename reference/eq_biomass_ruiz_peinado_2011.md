# Biomass equations for Spanish softwood species

Allometric equations adjusted for Spanish softwood species

## Usage

``` r
eq_biomass_ruiz_peinado_2011(species, component = "stem", return_rmse = FALSE)
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

**Supported species (10):**

*Abies alba*, *Abies pinsapo*, *Juniperus thurifera*, *Pinus
canariensis*, *Pinus halepensis*, *Pinus nigra*, *Pinus pinaster*,
*Pinus pinea*, *Pinus sylvestris*, *Pinus uncinata*

**Available components:**

Aboveground / belowground groups (summed automatically):

- `"AGB"` — total aboveground biomass

- `"BGB"` — total belowground biomass (roots)

- `"tree"` — total tree biomass (AGB + BGB)

Tree structural groups:

- `"stem"` — stem wood

- `"branches"` — all branch fractions combined

- `"roots"` — roots (equivalent to BGB)

Individual tree components (species availability varies):

- `"thick branches"` — branches \> 7 cm

- `"thick and medium branches"` — branches \> 2 cm

- `"medium branches"` — branches 2–7 cm

- `"small branches and leaves"` — branches \< 2 cm including
  leaves/needles

Users can check the full species–component matrix in
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md).

## See also

[`silv_predict_biomass()`](https://cidree.github.io/silviculture/reference/silv_predict_biomass.md),
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md),
[`eq_biomass_montero_2005()`](https://cidree.github.io/silviculture/reference/eq_biomass_montero_2005.md),
[`eq_biomass_dieguez_aranda_2009()`](https://cidree.github.io/silviculture/reference/eq_biomass_dieguez_aranda_2009.md),
[`eq_biomass_ruiz_peinado_2012()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2012.md),
[`eq_biomass_manrique_2017()`](https://cidree.github.io/silviculture/reference/eq_biomass_manrique_2017.md),
[`eq_biomass_menendez_2022()`](https://cidree.github.io/silviculture/reference/eq_biomass_menendez_2022.md),
[`eq_biomass_cudjoe_2024()`](https://cidree.github.io/silviculture/reference/eq_biomass_cudjoe_2024.md)

## Examples

``` r
## Aboveground biomass for Pinus pinaster
eq_biomass_ruiz_peinado_2011("Pinus pinaster", "AGB")
#> <silviculture::ModelBiomass>
#>  @ equation  : chr "ruiz-peinado-2011"
#>  @ species   : chr "Pinus pinaster"
#>  @ component : chr "AGB"
#>  @ expression:'data.frame':  3 obs. of  2 variables:
#>  .. $ expression: chr  "0.0278 * d^2.115 * h^0.618" "0.000381 * d^3.141" "0.0129 * d^2.320"
#>  .. $ species   : chr  "Pinus pinaster" "Pinus pinaster" "Pinus pinaster"
#>  @ url       : chr "https://doi.org/10.5424/fs/2011201-11643"
#>  @ obs       : chr "Diameter is assumed to in centimeters, and height is assumed to be in meters"
#>  @ params    :List of 3
#>  .. $ return_rmse: logi FALSE
#>  .. $ comp       : chr [1:3] "stem" "thick and medium branches" "small branches and leaves"
#>  .. $ rmse       : num [1:3] 14.47 7.04 7.67
```
