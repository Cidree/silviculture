# Calculate Tree Biomass

Computes the biomass of a tree species using species-specific allometric
equations (in kg). Currently, only equations for Spain are available.

## Usage

``` r
silv_predict_biomass(
  diameter = NULL,
  height = NULL,
  model,
  ntrees = NULL,
  rcd = NULL,
  bp = NULL,
  quiet = FALSE
)
```

## Arguments

- diameter:

  A numeric vector of tree diameters at breast height (in cm).

- height:

  A numeric vector of tree heights (in m).

- model:

  A function. A function with the structure `eq_biomass_*()` with
  additional arguments depending on the model used.

- ntrees:

  An optional numeric value indicating the number of trees in this
  diameter-height class. Defaults to 1 if `NULL`.

- rcd:

  An optional numeric vector of root collar diameters (in cm). Required
  for
  [`eq_biomass_menendez_2022`](https://cidree.github.io/silviculture/reference/eq_biomass_menendez_2022.md),
  which uses root collar diameter instead of diameter at breast height.
  Defaults to `diameter` if `NULL`.

- bp:

  An optional numeric vector of biomass packing values (in m³). Required
  for a subset of species in
  [`eq_biomass_menendez_2022`](https://cidree.github.io/silviculture/reference/eq_biomass_menendez_2022.md)
  (e.g. *Pinus halepensis*, *Pinus nigra*, *Quercus suber*, *Evergreen
  broadleaves*).

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.

## Value

A numeric vector with predicted tree biomass (kg).

## Details

The function estimates biomass using validated allometric models
available in the dataset
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md).
The available models include:

- **[`eq_biomass_ruiz_peinado_2011()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2011.md)**:
  Developed for softwood species in Spain.

- **[`eq_biomass_ruiz_peinado_2012()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2012.md)**:
  Developed for hardwood species in Spain.

- **[`eq_biomass_montero_2005()`](https://cidree.github.io/silviculture/reference/eq_biomass_montero_2005.md)**:
  Developed for 35 Spanish species.

- **[`eq_biomass_dieguez_aranda_2009()`](https://cidree.github.io/silviculture/reference/eq_biomass_dieguez_aranda_2009.md)**:
  Developed for 7 Galician species.

- **[`eq_biomass_manrique_2017()`](https://cidree.github.io/silviculture/reference/eq_biomass_manrique_2017.md)**:
  Developed for *Quercus petraea* and *Quercus pyrenaica*.

- **[`eq_biomass_menendez_2022()`](https://cidree.github.io/silviculture/reference/eq_biomass_menendez_2022.md)**:
  Developed for young plantations (\< 30 years) of 18 Spanish species.
  Uses `rcd` (root collar diameter) instead of `diameter`; some species
  require `bp` (biomass packing) instead of `rcd`.

- **[`eq_biomass_cudjoe_2024()`](https://cidree.github.io/silviculture/reference/eq_biomass_cudjoe_2024.md)**:
  Developed for *Pinus sylvestris* and *Quercus petraea* in Castille and
  León, Spain.

Users can check the list of supported species and their corresponding
components in
[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md).

If you would like to suggest additional models, please open a new issue
on GitHub.

## See also

[biomass_models](https://cidree.github.io/silviculture/reference/biomass_models.md),
[`eq_biomass_montero_2005()`](https://cidree.github.io/silviculture/reference/eq_biomass_montero_2005.md),
[`eq_biomass_dieguez_aranda_2009()`](https://cidree.github.io/silviculture/reference/eq_biomass_dieguez_aranda_2009.md),
[`eq_biomass_ruiz_peinado_2011()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2011.md),
[`eq_biomass_ruiz_peinado_2012()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2012.md),
[`eq_biomass_manrique_2017()`](https://cidree.github.io/silviculture/reference/eq_biomass_manrique_2017.md),
[`eq_biomass_menendez_2022()`](https://cidree.github.io/silviculture/reference/eq_biomass_menendez_2022.md),
[`eq_biomass_cudjoe_2024()`](https://cidree.github.io/silviculture/reference/eq_biomass_cudjoe_2024.md)

## Examples

``` r
# 1. Vector-based calculation: predict stem/tree biomass for Pinus pinaster
model <- eq_biomass_ruiz_peinado_2011("Pinus pinaster")
predicted_biomass <- silv_predict_biomass(
  diameter = c(20, 25, 30),
  height   = c(15, 17, 18),
  model    = model
)
#> ! Cite this model using <https://doi.org/10.5424/fs/2011201-11643>
#> ℹ Diameter is assumed to in centimeters, and height is assumed to be in meters
print(predicted_biomass)
#> [1]  83.66574 144.91233 220.75709

# 2. Dataset-based tutorial: apply to a forest inventory data frame
inventory <- data.frame(
  tree_id  = 1:3,
  species  = c("Pinus pinaster", "Pinus pinaster", "Pinus pinaster"),
  dbh_cm   = c(18.5, 22.1, 29.4),
  height_m = c(14.0, 16.5, 19.0)
)

# Apply prediction and append a new column to the dataset
inventory$biomass_kg <- silv_predict_biomass(
  diameter = inventory$dbh_cm,
  height   = inventory$height_m,
  model    = model
)
#> ! Cite this model using <https://doi.org/10.5424/fs/2011201-11643>
#> ℹ Diameter is assumed to in centimeters, and height is assumed to be in meters
print(inventory)
#>   tree_id        species dbh_cm height_m biomass_kg
#> 1       1 Pinus pinaster   18.5     14.0    67.9861
#> 2       2 Pinus pinaster   22.1     16.5   109.6073
#> 3       3 Pinus pinaster   29.4     19.0   218.7102

# 3. Young plantation example (Menendez 2022 model) using rcd and bp
# Menendez 2022 equations use root collar diameter (rcd) and/or biomass packing (bp)
model_menendez <- eq_biomass_menendez_2022("Pinus pinaster")
predicted_young_pinaster <- silv_predict_biomass(
  rcd      = c(5.2, 7.1, 9.4),   # Root collar diameter in cm
  height   = c(2.1, 3.2, 4.5),   # Height in m
  model    = model_menendez
)
#> ! Cite this model using <https://doi.org/10.1016/j.biombioe.2022.106453>
#> ℹ AGB is the aboveground dry biomass or aerial biomass (kg), RCD is the root-collar-diameter (cm), h is the total tree height (m), CPA is the crown projection area (m2), BP is the biomass packing (m3)
print(predicted_young_pinaster)
#> [1]  2.152248  4.972754 10.253238

# For Pinus halepensis, Menendez 2022 requires biomass packing (bp)
model_halepensis <- eq_biomass_menendez_2022("Pinus halepensis")
predicted_young_halepensis <- silv_predict_biomass(
  bp     = c(0.005, 0.012),     # Biomass packing in m3
  model  = model_halepensis
)
#> ! Cite this model using <https://doi.org/10.1016/j.biombioe.2022.106453>
#> ℹ AGB is the aboveground dry biomass or aerial biomass (kg), RCD is the root-collar-diameter (cm), h is the total tree height (m), CPA is the crown projection area (m2), BP is the biomass packing (m3)
print(predicted_young_halepensis)
#> [1] 0.01750195 0.03701001
```
