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

**Supported species (13):**

*Alnus glutinosa*, *Castanea sativa*, *Ceratonia siliqua*, *Eucalyptus
globulus*, *Fagus sylvatica*, *Fraxinus angustifolia*, *Olea europaea*,
*Populus x euramericana*, *Quercus canariensis*, *Quercus faginea*,
*Quercus ilex*, *Quercus pyrenaica*, *Quercus suber*

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

- `"stem and thick branches"` — stem together with branches \> 7 cm

- `"thick branches"` — branches \> 7 cm

- `"thick and medium branches"` — branches \> 2 cm

- `"medium branches"` — branches 2–7 cm

- `"small branches"` — branches 0.5–2 cm

- `"small branches and leaves"` — branches \< 2 cm including leaves

- `"medium branches, small branches and leaves"` — branches \< 7 cm
  including leaves

Users can check the full species–component matrix in
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md).

## See also

[`silv_predict_biomass()`](https://cidree.github.io/silviculture/reference/silv_predict_biomass.md),
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md),
[`eq_biomass_montero_2005()`](https://cidree.github.io/silviculture/reference/eq_biomass_montero_2005.md),
[`eq_biomass_dieguez_aranda_2009()`](https://cidree.github.io/silviculture/reference/eq_biomass_dieguez_aranda_2009.md),
[`eq_biomass_ruiz_peinado_2011()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2011.md),
[`eq_biomass_manrique_2017()`](https://cidree.github.io/silviculture/reference/eq_biomass_manrique_2017.md),
[`eq_biomass_menendez_2022()`](https://cidree.github.io/silviculture/reference/eq_biomass_menendez_2022.md),
[`eq_biomass_cudjoe_2024()`](https://cidree.github.io/silviculture/reference/eq_biomass_cudjoe_2024.md)

## Examples

``` r
## Aboveground biomass for Quercus suber
eq_biomass_ruiz_peinado_2012("Quercus suber", "AGB")
#> <silviculture::ModelBiomass>
#>  @ equation  : chr "ruiz-peinado-2012"
#>  @ species   : chr "Quercus suber"
#>  @ component : chr "AGB"
#>  @ expression:'data.frame':  4 obs. of  2 variables:
#>  .. $ expression: chr  "0.00525 * d^2 * h + 0.278 * d * h" "0.0135 * d^2 * h" "0.127 * d * h" "0.0463 * d * h"
#>  .. $ species   : chr  "Quercus suber" "Quercus suber" "Quercus suber" "Quercus suber"
#>  @ url       : chr "http://dx.doi.org/10.5424/fs/2112211-02193"
#>  @ obs       : chr "Diameter is assumed to in centimeters, and height is assumed to be in meters"
#>  @ params    :List of 3
#>  .. $ return_rmse: logi FALSE
#>  .. $ comp       : chr [1:4] "stem" "thick branches" "medium branches" "small branches and leaves"
#>  .. $ rmse       : num [1:4] 66.87 110.76 26.47 8.55
```
