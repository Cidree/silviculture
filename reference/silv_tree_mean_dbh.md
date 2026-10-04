# Calculate Mean DBH or derive it from Perimeter

Calculates the mean diameter at breast height (DBH) from two
perpendicular measurements or derives it from the perimeter.

## Usage

``` r
silv_tree_mean_dbh(dbh1 = NULL, dbh2 = NULL, perimeter = NULL, units = "cm")
```

## Arguments

- dbh1:

  A numeric vector with the first DBH measurement.

- dbh2:

  A numeric vector with the second DBH measurement.

- perimeter:

  A numeric vector with the perimeter measurement.

- units:

  The units of the inputs (one of `mm`, `cm`, `dm`, or `m`).

## Value

A numeric vector with the mean DBH.

## Examples

``` r
silv_tree_mean_dbh(dbh1 = 20, dbh2 = 22)
#> [1] 21
silv_tree_mean_dbh(perimeter = 65)
#> [1] 20.69014
```
