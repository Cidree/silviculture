# Classify diameters in classes

**\[deprecated\]**

Classifies the measured diameters into classes of a specified length

## Usage

``` r
silv_diametric_class(
  diameter,
  dmin = 7.5,
  dmax = NULL,
  class_length = 5,
  include_lowest = TRUE,
  return_intervals = FALSE
)
```

## Arguments

- diameter:

  A numeric vector of diameters

- dmin:

  The minimum inventory diameter in centimeters

- dmax:

  The maximum inventory diameter in centimeters. Values that are greater
  than `dmax` are included in the greatest class

- class_length:

  The length of the class in centimeters

- include_lowest:

  Logical. If TRUE (the default), the intervals are `[dim1, dim2)`. If
  FALSE, the intervals are `(dim1, dim2]`

  \[dim1, dim2)`. If FALSE, the intervals are `(dim1, dim2\]:
  R:dim1,%20dim2)%60.%20If%20FALSE,%20the%20intervals%20are%20%60(dim1,%20dim2

- return_intervals:

  If FALSE, it returns the intervals. Otherwise (the default), it
  returns the class center

## Value

A numeric vector

## Examples

``` r
library(dplyr)
inventory_samples |>
  mutate(dclass = silv_diametric_class(diameter))
#> # A tibble: 162 × 5
#>    plot_id species diameter height dclass
#>      <int>   <int>    <dbl>  <dbl>  <dbl>
#>  1       7      27     50.6   18.9     50
#>  2       7      27     57.2   19.8     55
#>  3       7      27     36.4   16.5     35
#>  4       7      27     46.4   18.5     45
#>  5       7      27     55.5   19.5     55
#>  6       7      27     59.5   17.7     60
#>  7       7      27     24.3   12.9     25
#>  8       7      27     50.5   16.6     50
#>  9       7      27     55.3   19.3     55
#> 10       7      27     48.6   18.5     50
#> # ℹ 152 more rows
```
