# Package index

## Predictions

Functions for estimating forest variables from models or equations

- [`silv_predict_biomass()`](https://cidree.github.io/silviculture/reference/silv_predict_biomass.md)
  : Calculate Tree Biomass
- [`silv_predict_height()`](https://cidree.github.io/silviculture/reference/silv_predict_height.md)
  : Estimates tree height from DBH
- [`silv_biomass()`](https://cidree.github.io/silviculture/reference/silv_biomass.md)
  **\[deprecated\]** : Calculate Tree Biomass

## Models and equations

Models and equations used in silv_predict\_\* functions

- [`eq_hd_vazquez_veloso_2025()`](https://cidree.github.io/silviculture/reference/eq_hd_vazquez_veloso_2025.md)
  : Estimates tree height from DBH
- [`eq_biomass_cudjoe_2024()`](https://cidree.github.io/silviculture/reference/eq_biomass_cudjoe_2024.md)
  : Biomass equations for 2 species in Castille and León (Spain)
- [`eq_biomass_dieguez_aranda_2009()`](https://cidree.github.io/silviculture/reference/eq_biomass_dieguez_aranda_2009.md)
  : Biomass equations for Galician species
- [`eq_biomass_manrique_2017()`](https://cidree.github.io/silviculture/reference/eq_biomass_manrique_2017.md)
  : Biomass equations two Quercus species
- [`eq_biomass_menendez_2022()`](https://cidree.github.io/silviculture/reference/eq_biomass_menendez_2022.md)
  : Biomass equations for young Spanish plantations
- [`eq_biomass_montero_2005()`](https://cidree.github.io/silviculture/reference/eq_biomass_montero_2005.md)
  : Biomass equations for Spanish species
- [`eq_biomass_ruiz_peinado_2011()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2011.md)
  : Biomass equations for Spanish softwood species
- [`eq_biomass_ruiz_peinado_2012()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2012.md)
  : Biomass equations for Spanish hardwood species

## Tree level metrics

Metrics to be calculated tree by tree

- [`silv_tree_dclass()`](https://cidree.github.io/silviculture/reference/silv_tree_dclass.md)
  : Classify diameters in classes
- [`silv_tree_basal_area()`](https://cidree.github.io/silviculture/reference/silv_tree_basal_area.md)
  : Calculates Basal Area
- [`silv_tree_volume()`](https://cidree.github.io/silviculture/reference/silv_tree_volume.md)
  : Calculate Tree Volume
- [`silv_basal_area()`](https://cidree.github.io/silviculture/reference/silv_basal_area.md)
  **\[deprecated\]** : Calculates Basal Area
- [`silv_diametric_class()`](https://cidree.github.io/silviculture/reference/silv_diametric_class.md)
  **\[deprecated\]** : Classify diameters in classes
- [`silv_volume()`](https://cidree.github.io/silviculture/reference/silv_volume.md)
  **\[deprecated\]** : Calculate Tree Volume

## Stand level metrics

Metrics to be calculated per stand or plot of trees

- [`silv_stand_basal_area()`](https://cidree.github.io/silviculture/reference/silv_stand_basal_area.md)
  : Calculates Basal Area
- [`silv_stand_dominant_diameter()`](https://cidree.github.io/silviculture/reference/silv_stand_dominant_diameter.md)
  : Calculates the dominant diameter
- [`silv_stand_dominant_height()`](https://cidree.github.io/silviculture/reference/silv_stand_dominant_height.md)
  : Calculates the dominant height
- [`silv_stand_lorey_height()`](https://cidree.github.io/silviculture/reference/silv_stand_lorey_height.md)
  : Calculates Lorey's Height
- [`silv_stand_qmean_diameter()`](https://cidree.github.io/silviculture/reference/silv_stand_qmean_diameter.md)
  : Calculates the quadratic mean diameter (QMD)
- [`silv_summary()`](https://cidree.github.io/silviculture/reference/silv_summary.md)
  : Calculates a bunch of forest metrics
- [`silv_dominant_height()`](https://cidree.github.io/silviculture/reference/silv_dominant_height.md)
  **\[deprecated\]** : Calculates the dominant height
- [`silv_lorey_height()`](https://cidree.github.io/silviculture/reference/silv_lorey_height.md)
  **\[deprecated\]** : Calculates Lorey's Height
- [`silv_ntrees_ha()`](https://cidree.github.io/silviculture/reference/silv_ntrees_ha.md)
  **\[deprecated\]** : Calculates number of trees per hectare
- [`silv_sqrmean_diameter()`](https://cidree.github.io/silviculture/reference/silv_sqrmean_diameter.md)
  **\[deprecated\]** : Calculates the quadratic mean diameter (QMD)

## Stand density metrics

Stand density and competition metrics

- [`silv_density_ntrees_ha()`](https://cidree.github.io/silviculture/reference/silv_density_ntrees_ha.md)
  : Calculates number of trees per hectare
- [`silv_density_hart()`](https://cidree.github.io/silviculture/reference/silv_density_hart.md)
  : Hart or Hart-Becking spacing index
- [`silv_density_sdi()`](https://cidree.github.io/silviculture/reference/silv_density_sdi.md)
  : Calculates the Stand Density Index
- [`silv_spacing_index()`](https://cidree.github.io/silviculture/reference/silv_spacing_index.md)
  **\[deprecated\]** : Hart or Hart-Becking spacing index
- [`silv_ntrees_ha()`](https://cidree.github.io/silviculture/reference/silv_ntrees_ha.md)
  **\[deprecated\]** : Calculates number of trees per hectare

## Inventory Sample Size

Calculate and explore forest inventory sample size

- [`silv_sample_size_simple()`](https://cidree.github.io/silviculture/reference/silv_sample_size_simple.md)
  : Calculates sample size for a simple random sampling (SRS)
- [`silv_sample_size_stratified()`](https://cidree.github.io/silviculture/reference/silv_sample_size_stratified.md)
  : Calculates sample size for a stratified sampling
- [`silv_sample_size()`](https://cidree.github.io/silviculture/reference/silv_sample_size.md)
  **\[deprecated\]** : Calculates sample size for a random sampling
  inventory
- [`plot_SimpleSampleSize`](https://cidree.github.io/silviculture/reference/SimpleSampleSize.md)
  : Plot Sample Size vs Error

## Treatments

Functions for forest treatments

- [`silv_treatment_thinning()`](https://cidree.github.io/silviculture/reference/silv_treatment_thinning.md)
  : Calculate Forestry Thinning Schemes

## LiDAR metrics

Extract and calculate forest structural metrics using LiDAR data

- [`lid_fcov()`](https://cidree.github.io/silviculture/reference/lid_fcov.md)
  : Calculate Forest Fraction Cover from LiDAR Data
- [`lid_lhdi()`](https://cidree.github.io/silviculture/reference/lid_lhdi.md)
  : LiDAR-derived Height Diversity Index (LHDI)

## Data

Datasets included in the package

- [`biomass_models`](https://cidree.github.io/silviculture/reference/biomass_models.md)
  : Biomass models
- [`inventory_samples`](https://cidree.github.io/silviculture/reference/inventory_samples.md)
  : Forest inventory samples

## Objects

S7 methods

- [`plot()`](https://cidree.github.io/silviculture/reference/plot.md) :
  Plot an object
