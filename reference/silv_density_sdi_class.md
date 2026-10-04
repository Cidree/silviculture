# Classifies the Stand Density Index

Classifies the Stand Density Index (SDI) into density classes or
calculates the relative SDI percentage based on USDA thresholds.

## Usage

``` r
silv_density_sdi_class(sdi, max_sdi, classify = TRUE)
```

## Arguments

- sdi:

  A numeric vector representing the Stand Density Index.

- max_sdi:

  A numeric vector representing the maximum SDI for the species/site.

- classify:

  A logical value indicating whether to classify the values into density
  classes (default is `TRUE`). If `FALSE`, it returns the relative SDI
  as a percentage.

## Value

A character vector with the density classes if `classify = TRUE`, or a
numeric vector with the relative SDI percentage if `classify = FALSE`.

## Details

The option `classify = TRUE` will use the `max_sdi` value to classify
the SDI into four competitive and growth conditions: low density
(\<24%), moderate density (24-35%), high density (34-55%), and extremely
high density (\>55%).

## References

USDA Forest Service. (n.d.). Stand Density Index.
https://www.fs.usda.gov/Internet/FSE_DOCUMENTS/stelprdb5270993.pdf

## Examples

``` r
## calculate SDI for a Pinus sylvestris stand (max 990)
sdi_val <- silv_density_sdi(ntrees = 800, dg = 23.4)

## check base classification
silv_density_sdi_class(sdi = sdi_val, max_sdi = 990)
#> [1] "Extremely high density"

## get relative SDI percentage
silv_density_sdi_class(sdi = sdi_val, max_sdi = 990, classify = FALSE)
#> [1] 70.84156
```
