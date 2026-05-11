# LiDAR-derived Height Diversity Index (LHDI)

LiDAR metric that calculates the LiDAR Height Diversity Index, which can
be used in `lidR` `*_metrics` functions

## Usage

``` r
lid_lhdi(z, interval = 0.5)
```

## Arguments

- z:

  coordinate Z (height) of the point

- interval:

  height of the intervals to calculate the metric

## Value

numeric

## References

Listopad, C. M. C. S., Masters, R. E., Drake, J., Weishampel, J., &
Branquinho, C. (2015). Structural diversity indices based on airborne
LiDAR as ecological indicators for managing highly dynamic landscapes.
Ecological Indicators, 57, 268–279.
[doi:10.1016/j.ecolind.2015.04.017](https://doi.org/10.1016/j.ecolind.2015.04.017)

## Examples

``` r
z <- c(0.5, 1.2, 1.8, 2.4, 3.1)
lid_lhdi(z)
#> [1] 0.9656627
```
