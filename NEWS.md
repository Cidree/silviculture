
# silviculture 0.2.0 (dev)

This new version brings new naming conventions that will be useful for sorting the package into "modules" of related functions.

* `silv_tree_*()`: tree-level metrics (although some can be also used as stand-level using the `ntrees` argument).

* `silv_stand_*()`: stand or plot-level metrics

* `silv_predict_*()`: predictions based on models

The old functions are now deprecated and will be eliminated in a future release.

## New functions

* `silv_density_sdi()`: calculates the Stand Density Index

## Bug Fixes

* Fix an error with the validator of variable names in `silviculture::Inventory` S7 class.

* `biomass_models`: some were failing because the "-" sign was parsed as an em dash.

## Enhancements

* `silv_volume()`: it assumed diameter to be in meters. Now the diameter must be given in centimeters. An informing message was added to the function.

## Deprecated functions

* `silv_diametric_class()` deprecated in favour of `silv_tree_dclass()`

* `silv_basal_area()` deprecated in favour of `silv_tree_basal_area()`

* `silv_volume()` deprecated in favour of `silv_tree_volume()`

* `silv_dominant_height()` deprecated in favour of `silv_stand_dominant_height()`

* `silv_lorey_height()` deprecated in favour of `silv_stand_lorey_height()`

* `silv_sqrmean_diameter()` deprecated in favour of `silv_stand_qmean_diameter()`


* `silv_spacing_index()` deprecated in favour of `silv_density_hart()`

* `silv_ntrees_ha()` deprecated in favour of `silv_density_ntrees_ha()`

* `silv_biomass()` deprecated in favour of `silv_predict_biomass()`

# silviculture 0.1.0

* Initial CRAN submission.
