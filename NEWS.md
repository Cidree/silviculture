
# Development version

## New functions

- `silv_predict_carbon()`: converts biomass estimates to carbon stock for a given
  species, component, and carbon model.

- `silv_predict_carbon_auto()`: automatically selects the best available carbon
  model for a species, with genus-level fallback when species-level data are
  unavailable.

- `silv_predict_snfi_volume()`: predicts tree volume using allometric coefficients
  from the Spanish National Forest Inventory (SNFI3 and SNFI4) by province and
  species.

- `silv_snfi_provinces()`: returns a lookup table of SNFI province codes and
  names.

- `silv_snfi_species()`: returns a lookup table of SNFI species codes and names
  for a given inventory version (`"SNFI3"` or `"SNFI4"`).

- `silv_predict_biomass_auto()`: automatically selects the best available biomass
  model for a given species and computes above- and below-ground biomass.

- `silv_predict_biomass_components()`: returns a per-component biomass breakdown
  (e.g. stem, bark, branches, roots) for a given species and model.

## Enhancements

- `silv_predict_biomass()`: extended to support all 7 allometric models. New
  `rcd` (root collar diameter) and `bp` (bark proportion) arguments added;
  `rcd` defaults to `diameter` when not supplied.

- `eq_biomass_cudjoe_2024()`: fixed `equation` slot returning `"cudjoe-2017"`
  instead of `"cudjoe-2024"`.

## Bug fixes

- `silv_predict_biomass_components()`: AGB/BGB column names are now correctly
  capitalised; the function now aborts with a clear message when the requested
  species is not supported by the chosen model.

- `silv_predict_biomass_components()`: informative error messages added when
  BGB or total-tree components are missing for a given species/model combination.

- `eq_biomass_ruiz_peinado_2012()`: the `equation` slot was incorrectly returning
  `"ruiz-peinado-2011"` instead of `"ruiz-peinado-2012"`.

## Data updates

- New datasets `snfi3_volume_coefficients` and `snfi4_volume_coefficients` with
  allometric volume coefficients from the 3rd and 4th Spanish National Forest
  Inventories, stored as compressed `.rda` files.

- New dataset `carbon_models` (264 x 13), with full documentation.

- `biomass_models` rebuilt (427 × 15, 0 parse errors) from corrected source spreadsheet.

## Testing

- Test suite modularised: the monolithic `test-dendrometry.R` has been split into
  focused files (`test-tree-level.R`, `test-stand-level.R`, `test-stand-density.R`,
  `test-lidar.R`, `test-sample-size.R`, `test-summary-thinning.R`) with
  comprehensive new coverage and fixed S7 class checks.

# silviculture 0.2.0

This new version brings new naming conventions that will be useful for sorting the package into "modules" of related functions.

* `silv_tree_*()`: tree-level metrics (although some can be also used as stand-level using the `ntrees` argument).

* `silv_stand_*()`: stand or plot-level metrics

* `silv_predict_*()`: predictions based on models

The old functions are now deprecated and will be eliminated in a future release.

## New functions

* `silv_density_sdi()`: calculates the Stand Density Index

* `silv_predict_height()`: estimates height from diameter, using the so-called h-d curves. The argument `equation` allows to choose which equations to use. Currently, only `eq_hd_aitor2025()` available.

* `silv_stand_dominant_diameter()`: calculates dominant diameter using two methods:

    - `Assman`: the mean diameter of the 100 thickest trees per hectare

    - `Weise`: the quadratic mean diameter of the 20% thickest trees per hectare

* `eq_biomass_*()`: equations to be used inside the `model` argument of `silv_predict_biomass()`.

## Bug Fixes

* Fix an error with the validator of variable names in `silviculture::Inventory` S7 class.

* `biomass_models`: some were failing because the "-" sign was parsed as an em dash.

## Enhancements

* Prediction functions (`silv_predict_*()`) will now have common arguments, and specific arguments that depend of the model used that are specified as a function (e.g. `silv_predict_height(model = eq_hd_aitor2025())`).

* `silv_volume()`: it assumed diameter to be in meters. Now the diameter must be given in centimeters. An informing message was added to the function.

* S7 `silviculture::Inventory` class now stores groups.

## Deprecated functions

* `silv_diametric_class()` deprecated in favour of `silv_tree_dclass()`

* `silv_basal_area()` deprecated in favour of `silv_tree_basal_area()` and `silv_stand_basal_area()`

* `silv_volume()` deprecated in favour of `silv_tree_volume()`

* `silv_dominant_height()` deprecated in favour of `silv_stand_dominant_height()`

* `silv_lorey_height()` deprecated in favour of `silv_stand_lorey_height()`

* `silv_sqrmean_diameter()` deprecated in favour of `silv_stand_qmean_diameter()`

* `silv_spacing_index()` deprecated in favour of `silv_density_hart()`

* `silv_ntrees_ha()` deprecated in favour of `silv_density_ntrees_ha()`

* `silv_biomass()` deprecated in favour of `silv_predict_biomass()`

# silviculture 0.1.0

* Initial CRAN submission.
