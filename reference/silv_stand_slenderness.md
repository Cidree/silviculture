# Calculates the Stand Slenderness Index

The Stand Slenderness Index is defined as the ratio of height to
diameter, multiplied by 100. It can be applied to mean height and
quadratic mean diameter, or dominant height and dominant diameter.

## Usage

``` r
silv_stand_slenderness(height, diameter)
```

## Arguments

- height:

  Numeric vector of tree heights

- diameter:

  Numeric vector of diameters or diameter classes

## Value

A numeric vector representing the slenderness index.

## Details

The formula used is: \$\$Slenderness = \frac{H}{D} \times 100\$\$ where
H is the height in meters and D is the diameter in centimeters.

## Examples

``` r
## Calculate slenderness using mean height and quadratic mean diameter
silv_stand_slenderness(height = 18, diameter = 25)
#> [1] 72
```
