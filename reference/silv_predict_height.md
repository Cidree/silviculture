# Estimates tree height from DBH

Estimates total tree height using height-diameter (h-d) equations.
Currently, only models developed for Spain are available.

## Usage

``` r
silv_predict_height(diameter, model, quiet = FALSE)
```

## Arguments

- diameter:

  Numeric vector with diameters in cm

- model:

  A function. A function with the structure `eq_hd_*()` with additional
  arguments depending on the specific model. Currently only
  [`eq_hd_vazquez_veloso_2025()`](https://cidree.github.io/silviculture/reference/eq_hd_vazquez_veloso_2025.md)
  is available.

- quiet:

  A logical value. If TRUE, suppresses any informational messages.

## Value

A numeric vector with predicted heights

## Details

The function estimates total tree height (in meters) using diameter at
breast height (in centimeters), and may require additional information
depending on the specific model. See each model’s documentation for
details.

## References

References for the models available:

- **[`eq_hd_vazquez_veloso_2025()`](https://cidree.github.io/silviculture/reference/eq_hd_vazquez_veloso_2025.md)**:
  Vázquez-Veloso, A., Yang, S.-I., Bullock, B.P., Bravo, F., 2025. One
  model to rule them all: A nationwide height–diameter model for 91
  Spanish forest species. Forest Ecology and Management 595, 122981.
  https://doi.org/10.1016/j.foreco.2025.122981

## See also

[`eq_hd_vazquez_veloso_2025()`](https://cidree.github.io/silviculture/reference/eq_hd_vazquez_veloso_2025.md)

## Examples

``` r
1 + 1 #TODO
#> [1] 2
```
