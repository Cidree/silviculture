
# Development version

## New functions

- `silv_predict_carbon()`: converts biomass estimates to carbon stock for a given
  species, component, and carbon model. (#19)

- `silv_predict_carbon_auto()`: automatically selects the best available carbon
  model for a species, with genus-level fallback when species-level data are
  unavailable. (#19)

- `silv_predict_snfi_volume()`: predicts tree volume using allometric coefficients
  from the Spanish National Forest Inventory (SNFI3 and SNFI4) by province and
  species. (#20)

- `silv_snfi_provinces()`: returns a lookup table of SNFI province codes and
  names. (#20)

- `silv_snfi_species()`: returns a lookup table of SNFI species codes and names
  for a given inventory version (`"SNFI3"` or `"SNFI4"`). (#20)

- `silv_predict_biomass_auto()`: automatically selects the best available biomass
  model for a given species and computes above- and below-ground biomass. (#18)

- `silv_predict_biomass_components()`: returns a per-component biomass breakdown
  (e.g. stem, bark, branches, roots) for a given species and model. (#18)

- `silv_tree_expansion_factor()`: dynamic expansion factors for fixed-area or
  concentric SNFI plots (radii 5, 10, 15, 25 m). (#27)

- `silv_tree_bal()` and `silv_tree_bas()`: Basal Area in Larger/Smaller trees
  competition indices per plot. (#27)

- `silv_tree_crown_ratio()`, `silv_tree_coordinates()`, `silv_tree_circumference()`,
  `silv_tree_mean_dbh()`, `silv_tree_slenderness()`: new tree-level metric functions
  adapted from SNFI support scripts. (#27)

- `silv_density_sdi_auto()`: automatically computes Reineke's Stand Density Index
  (SDI) and selects the correct `beta` coefficient via a cascading fallback
  (exact match -> region -> country -> genus -> default Reineke -1.605). Also
  returns `sdimax` and `sdimax_model` via SDImax auto-selection, with optional
  `classify = TRUE` for qualitative density classes. (#23, #26)

- `silv_density_sdimax()`: computes Maximum Stand Density Index (SDImax) using
  Rodríguez de Prado (2020) models, including climate-dependent variants. (#25)

- `silv_stand_slenderness()`: computes the stand-level slenderness metric
  (H0/dg). (#28)

- `silv_summary_species()`: pivots stand metrics for the top N dominant species
  (`sp1`, `sp2`, `G_sp1`, `N_sp1`, ...). (#28)

- `silv_summary_mortality()`: summarizes stand metrics partitioned into alive
  vs dead trees (`G_alive`, `G_dead`, `N_alive`, `N_dead`). (#28)

- `silv_tree_summary()`: computes per-tree class (`dclass`), basal area (`g`),
  expansion factor (`expan`), expanded basal area (`g_ha`), slenderness, and
  competition indices (`bal`, `bas`), with optional on-the-fly volume, biomass,
  and carbon predictions (`vcc`, `vsc`, `iavc`, `biomass`, `carbon`). Supports
  concentric SNFI plots via `plot_shape = "snfi"`. (#29)

## Enhancements

- `silv_predict_biomass()`: extended to support all 7 allometric models. New
  `rcd` (root collar diameter) and `bp` (bark proportion) arguments added;
  `rcd` defaults to `diameter` when not supplied. (#16)

- `eq_biomass_cudjoe_2024()`: fixed `equation` slot returning `"cudjoe-2017"`
  instead of `"cudjoe-2024"`. (#16)

- `silv_predict_height()`: now defaults to `eq_hd_vazquez_veloso_2025("All the
  species")` when no model is specified. (#21)

- `silv_predict_biomass()`: young-plantation support extended — root collar
  diameter (`rcd`) and biomass packing (`bp`) can now be used when DBH/diameter
  is absent (Menéndez 2022 models). (#21)

- S7 `plot` methods: documentation consolidated into a single `plot` generic
  page (`@rdname plot`); generic signatures aligned to fix R CMD check `codoc`
  mismatches. (#21)

- `silv_summary()`: now includes `slenderness`; extended to calculate and
  aggregate stand volume (`v_ha`, m3/ha), biomass (`w_ha`, t/ha), and carbon
  (`c_ha`, t/ha). (#28, #29)

- `silv_summary_species()`: extended with volume, biomass, and carbon columns
  (`V_sp1`, `W_sp1`, `C_sp1`). (#29)

- `silv_summary_mortality()`: extended with volume, biomass, and carbon columns
  split by alive/dead (`V_alive`/`V_dead`, `W_alive`/`W_dead`, `C_alive`/`C_dead`). (#29)

- `Inventory` S7 class validator: now accounts for the `slenderness` field. (#28)

## Bug fixes

- `silv_predict_biomass_components()`: AGB/BGB column names are now correctly
  capitalised; the function now aborts with a clear message when the requested
  species is not supported by the chosen model. (#18)

- `silv_predict_biomass_components()`: informative error messages added when
  BGB or total-tree components are missing for a given species/model combination. (#17)

- `eq_biomass_ruiz_peinado_2012()`: the `equation` slot was incorrectly returning
  `"ruiz-peinado-2011"` instead of `"ruiz-peinado-2012"`. (#14)

- `eq_hd_vazquez_veloso_2025()`: fixed the `b` coefficient referencing the
  wrong variable. (#13)

- `lid_lhdi()`: fixed precision issue in LiDAR height diversity index. (#22)

- Fixed dominant height / diameter validation and basal area multiplier
  calculation errors. (#22)

## Data updates

- New datasets `snfi3_volume_coefficients` and `snfi4_volume_coefficients` with
  allometric volume coefficients from the 3rd and 4th Spanish National Forest
  Inventories, stored as compressed `.rda` files. (#20)

- New dataset `carbon_models` (264 x 13), with full documentation. (#14)

- `biomass_models` rebuilt (427 × 15, 0 parse errors) from corrected source spreadsheet. (#14)

- New dataset `sdi_coefficients`: beta exponents by article, country, region,
  and species, used by `silv_density_sdi_auto()`. (#23)

- New dataset `sdimax_models` (88 x 13): reference ("basic") and
  climate-dependent SDImax parameters. (#25)



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
