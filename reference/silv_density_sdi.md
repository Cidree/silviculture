# Calculates the Stand Density Index

The Stand Density Index (SDI) is relationship between the average tree
size and density of trees per hectare.

## Usage

``` r
silv_density_sdi(ntrees, dg, classify = FALSE, max_sdi = NULL)
```

## Arguments

- ntrees:

  A numeric vector representing the number of trees per hectare

- dg:

  A numeric vector of quadratic mean diameters

- classify:

  whether to classify the values using USDA thresholds

- max_sdi:

  used when `classify = TRUE`. The maximum SDi, which depends on the
  species, stand type, and site

## Value

A numeric vector

## Details

The SDI has different interpretation depending on the species, location,
and also the management type (even-aged, uneven-aged...). The value of
maximum SDI must be determined from the literature and used carefully.
The option `classify = TRUE` will use this value to classify the SDI in
low density (\<24%), moderate density (24-35%), high density (34-55%),
and extremely high density (\>55%).

## Examples

``` r
## calculate SDI for a Pinus sulvestris stand (max 990)
silv_density_sdi(ntrees = 800, dg = 23.4, max_sdi = 990)
#> [1] 70.84156

## check base classification (other can be used)
silv_density_sdi(ntrees = 800, dg = 23.4, classify = TRUE, max_sdi = 990)
#> [1] "Extremely high density"
```
