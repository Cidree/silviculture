# Changelog

## silviculture 0.2.0

CRAN release: 2025-09-27

This new version brings new naming conventions that will be useful for
sorting the package into “modules” of related functions.

- `silv_tree_*()`: tree-level metrics (although some can be also used as
  stand-level using the `ntrees` argument).

- `silv_stand_*()`: stand or plot-level metrics

- `silv_predict_*()`: predictions based on models

The old functions are now deprecated and will be eliminated in a future
release.

### New functions

- [`silv_density_sdi()`](https://cidree.github.io/silviculture/reference/silv_density_sdi.md):
  calculates the Stand Density Index

- [`silv_predict_height()`](https://cidree.github.io/silviculture/reference/silv_predict_height.md):
  estimates height from diameter, using the so-called h-d curves. The
  argument `equation` allows to choose which equations to use.
  Currently, only `eq_hd_aitor2025()` available.

- [`silv_stand_dominant_diameter()`](https://cidree.github.io/silviculture/reference/silv_stand_dominant_diameter.md):
  calculates dominant diameter using two methods:

  - `Assman`: the mean diameter of the 100 thickest trees per hectare

  - `Weise`: the quadratic mean diameter of the 20% thickest trees per
    hectare

- `eq_biomass_*()`: equations to be used inside the `model` argument of
  [`silv_predict_biomass()`](https://cidree.github.io/silviculture/reference/silv_predict_biomass.md).

### Bug Fixes

- Fix an error with the validator of variable names in
  `silviculture::Inventory` S7 class.

- `biomass_models`: some were failing because the “-” sign was parsed as
  an em dash.

### Enhancements

- Prediction functions (`silv_predict_*()`) will now have common
  arguments, and specific arguments that depend of the model used that
  are specified as a function
  (e.g. `silv_predict_height(model = eq_hd_aitor2025())`).

- [`silv_volume()`](https://cidree.github.io/silviculture/reference/silv_volume.md):
  it assumed diameter to be in meters. Now the diameter must be given in
  centimeters. An informing message was added to the function.

- S7 `silviculture::Inventory` class now stores groups.

### Deprecated functions

- [`silv_diametric_class()`](https://cidree.github.io/silviculture/reference/silv_diametric_class.md)
  deprecated in favour of
  [`silv_tree_dclass()`](https://cidree.github.io/silviculture/reference/silv_tree_dclass.md)

- [`silv_basal_area()`](https://cidree.github.io/silviculture/reference/silv_basal_area.md)
  deprecated in favour of
  [`silv_tree_basal_area()`](https://cidree.github.io/silviculture/reference/silv_tree_basal_area.md)
  and
  [`silv_stand_basal_area()`](https://cidree.github.io/silviculture/reference/silv_stand_basal_area.md)

- [`silv_volume()`](https://cidree.github.io/silviculture/reference/silv_volume.md)
  deprecated in favour of
  [`silv_tree_volume()`](https://cidree.github.io/silviculture/reference/silv_tree_volume.md)

- [`silv_dominant_height()`](https://cidree.github.io/silviculture/reference/silv_dominant_height.md)
  deprecated in favour of
  [`silv_stand_dominant_height()`](https://cidree.github.io/silviculture/reference/silv_stand_dominant_height.md)

- [`silv_lorey_height()`](https://cidree.github.io/silviculture/reference/silv_lorey_height.md)
  deprecated in favour of
  [`silv_stand_lorey_height()`](https://cidree.github.io/silviculture/reference/silv_stand_lorey_height.md)

- [`silv_sqrmean_diameter()`](https://cidree.github.io/silviculture/reference/silv_sqrmean_diameter.md)
  deprecated in favour of
  [`silv_stand_qmean_diameter()`](https://cidree.github.io/silviculture/reference/silv_stand_qmean_diameter.md)

- [`silv_spacing_index()`](https://cidree.github.io/silviculture/reference/silv_spacing_index.md)
  deprecated in favour of
  [`silv_density_hart()`](https://cidree.github.io/silviculture/reference/silv_density_hart.md)

- [`silv_ntrees_ha()`](https://cidree.github.io/silviculture/reference/silv_ntrees_ha.md)
  deprecated in favour of
  [`silv_density_ntrees_ha()`](https://cidree.github.io/silviculture/reference/silv_density_ntrees_ha.md)

- [`silv_biomass()`](https://cidree.github.io/silviculture/reference/silv_biomass.md)
  deprecated in favour of
  [`silv_predict_biomass()`](https://cidree.github.io/silviculture/reference/silv_predict_biomass.md)

## silviculture 0.1.0

CRAN release: 2025-05-29

- Initial CRAN submission.
