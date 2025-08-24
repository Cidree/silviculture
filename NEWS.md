
# silviculture 0.1.0.9000 (dev)

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
