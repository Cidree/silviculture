# Calculate Forest Fraction Cover from LiDAR Data

This function calculates the forest fraction cover (Fcov) from LiDAR
data. The Fcov in LiDAR is defined as the proportion of first returns
above a specified height threshold (default: 5 meters) relative to the
total number of first returns.

## Usage

``` r
lid_fcov(z, rn, th = 5)
```

## Arguments

- z:

  A numeric vector representing the heights of LiDAR returns

- rn:

  An integer vector indicating the return number for each LiDAR point.
  First returns are identified by a value of `1`

- th:

  a numeric vector of length one specifying the height threshold

## Value

A numeric value representing the forest fraction cover, which is the
proportion of first returns with heights greater than 5 meters.

## Examples

``` r
# Example data
z <- c(2, 6, 10, 4, 15)
rn <- c(1, 1, 2, 1, 1)

# Calculate forest fraction cover
lid_fcov(z, rn)
#> [1] 0.5
```
