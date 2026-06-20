# Biomass equations for Galician species

Allometric equations adjusted for Galician (Spain) species

## Usage

``` r
eq_biomass_dieguez_aranda_2009(
  species,
  component = "stem",
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
  calculation (e.g., "tree", "stem", "branches"). See Details.

- return_r2:

  A logical value. If TRUE, the function returns the root mean squared
  error (RMSE) of the selected model instead of the biomass value.

- return_rmse:

  A logical value. If TRUE, the function returns the root mean squared
  error (RMSE) of the selected model instead of the biomass value.

## Value

A ModelBiomass object containing the configured model parameters and
expressions.

## Details

**Supported species (7):**

*Betula alba*, *Eucalyptus globulus*, *Eucalyptus nitens*, *Pinus
pinaster*, *Pinus radiata*, *Pinus sylvestris*, *Quercus robur*

**Available components:**

Aboveground / belowground groups (summed automatically):

- `"AGB"` — total aboveground biomass

- `"BGB"` — total belowground biomass (roots)

Tree structural groups:

- `"stem"` — stem fraction(s)

- `"branches"` — all branch fractions combined

- `"roots"` — roots (equivalent to BGB)

Individual tree components (species availability varies):

- `"stem and thick branches"` — stem together with thickest branches

- `"thick branches"` — branches \> 7 cm

- `"medium branches"` — branches 2–7 cm

- `"small branches"` — branches 0.5–2 cm

- `"twigs"` — branches \< 0.5 cm

- `"dry branches"` — dead attached branches

- `"leaves"` — foliage (including needles)

- `"roots"` — coarse roots

Note that total-tree equations (`"tree"` / `"all"`) are not available
for this model. Also, *Eucalyptus globulus*, *Eucalyptus nitens*, and
*Pinus pinaster* lack BGB equations (requesting `"BGB"` or `"roots"`
will fail).

Users can check all available species and components in the
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md)
dataset provided by the library.

## See also

[`silv_predict_biomass()`](https://cidree.github.io/silviculture/reference/silv_predict_biomass.md),
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md),
[`eq_biomass_montero_2005()`](https://cidree.github.io/silviculture/reference/eq_biomass_montero_2005.md),
[`eq_biomass_ruiz_peinado_2011()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2011.md),
[`eq_biomass_ruiz_peinado_2012()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2012.md),
[`eq_biomass_manrique_2017()`](https://cidree.github.io/silviculture/reference/eq_biomass_manrique_2017.md),
[`eq_biomass_menendez_2022()`](https://cidree.github.io/silviculture/reference/eq_biomass_menendez_2022.md),
[`eq_biomass_cudjoe_2024()`](https://cidree.github.io/silviculture/reference/eq_biomass_cudjoe_2024.md)

## Examples

``` r
## Aboveground biomass for Pinus pinaster
eq_biomass_dieguez_aranda_2009("Pinus pinaster", "AGB")
#> <silviculture::ModelBiomass>
#>  @ equation  : chr "dieguez-aranda-2009"
#>  @ species   : chr "Pinus pinaster"
#>  @ component : chr "AGB"
#>  @ expression:'data.frame':  6 obs. of  2 variables:
#>  .. $ expression: chr  "0.3882 + 0.01149 * (d ^ 2) * h" "0.0079 * (d ^ 2.098) * (h ^ 0.466)" "3.202 - 0.01484 * (d ^ 2) - 0.4228 * h + 0.00279 * (d ^ 2) * h" "0.09781 * (d ^ 2.288) * (h ^ (-0.9648))" ...
#>  .. $ species   : chr  "Pinus pinaster" "Pinus pinaster" "Pinus pinaster" "Pinus pinaster" ...
#>  @ url       : chr "https://www.researchgate.net/publication/312219888_Herramientas_selvicolas_para_la_gestion_forestal_sostenible_en_Galicia"
#>  @ obs       : chr "Diameter is assumed to in centimeters, and height is assumed to be in meters"
#>  @ params    :List of 5
#>  .. $ return_r2  : logi FALSE
#>  .. $ return_rmse: logi FALSE
#>  .. $ comp       : chr [1:6] "stem and thick branches" "bark" "thick branches" "medium branches" ...
#>  .. $ r2         : num [1:6] 0.908 0.943 0.806 0.826 0.678 0.823
#>  .. $ rmse       : num [1:6] 51.2 6.3 13.8 4.7 1.4 5.8
```
