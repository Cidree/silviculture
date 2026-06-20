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

A ModelBiomass object containing the configured model parameters and
expressions.

## Details

**Supported species (35):**

*Abies alba*, *Abies pinsapo*, *Alnus glutinosa*, *Betula* spp.,
*Castanea sativa*, *Ceratonia siliqua*, *Erica arborea*, *Eucalyptus*
spp., *Fagus sylvatica*, *Fraxinus* spp., *Ilex canariensis*, *Juniperus
oxycedrus*, *Juniperus phoenicea*, *Juniperus thurifera*, *Laurus
azorica*, *Myrica faya*, *Olea europaea* var. *sylvestris*, *Other
broadleaves*, *Other conifers*, *Other laurel species*, *Pinus
canariensis*, *Pinus halepensis*, *Pinus nigra*, *Pinus pinaster*,
*Pinus pinea*, *Pinus radiata*, *Pinus sylvestris*, *Pinus uncinata*,
*Populus x euramericana*, *Quercus canariensis*, *Quercus faginea*,
*Quercus ilex*, *Quercus pyrenaica*, *Quercus robur*, *Quercus suber*

**Available components:**

Aboveground / belowground groups (summed automatically):

- `"AGB"` — total aboveground biomass

- `"BGB"` — total belowground biomass (roots)

- `"all"` or `"tree"` — total tree biomass (AGB + BGB)

Tree structural groups:

- `"stem"` — stem fraction(s)

- `"branches"` — all branch fractions combined

- `"roots"` — roots (equivalent to BGB)

Individual tree components (species availability varies):

- `"stem"` — stem wood

- `"stem and thick branches"` — stem together with branches \> 7 cm

- `"thick branches"` — branches \> 7 cm

- `"medium branches"` — branches 2–7 cm

- `"small branches"` — branches \< 2 cm

- `"leaves"` — foliage (including needles)

- `"roots"` — coarse roots

Note that for this model, `"tree"` (or `"all"`) is an independent
regression equation fitted to total-tree data. It was **not** derived by
summing the AGB and BGB equations. Consequently, there is a numerical
discrepancy between the direct `"tree"` estimation and the sum of
separate `"AGB"` and `"BGB"` estimations (e.g. for *Pinus sylvestris* at
diameter = 20 cm and height = 10 m, the direct total is 89.1 kg, while
AGB + BGB is 115.9 kg, a 24% difference).

Also, the following 6 species have no BGB/roots equations in this model:
*Abies pinsapo*, *Erica arborea*, *Eucalyptus* spp., *Ilex canariensis*,
*Laurus azorica*, *Myrica faya* (requesting `"BGB"` or `"roots"` will
fail).

Users can check all available species and components in the
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md)
dataset provided by the library.

## See also

[`silv_predict_biomass()`](https://cidree.github.io/silviculture/reference/silv_predict_biomass.md),
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md),
[`eq_biomass_dieguez_aranda_2009()`](https://cidree.github.io/silviculture/reference/eq_biomass_dieguez_aranda_2009.md),
[`eq_biomass_ruiz_peinado_2011()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2011.md),
[`eq_biomass_ruiz_peinado_2012()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2012.md),
[`eq_biomass_manrique_2017()`](https://cidree.github.io/silviculture/reference/eq_biomass_manrique_2017.md),
[`eq_biomass_menendez_2022()`](https://cidree.github.io/silviculture/reference/eq_biomass_menendez_2022.md),
[`eq_biomass_cudjoe_2024()`](https://cidree.github.io/silviculture/reference/eq_biomass_cudjoe_2024.md)

## Examples

``` r
## Aboveground biomass for Pinus pinaster
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
