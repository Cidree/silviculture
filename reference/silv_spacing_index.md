# Hart or Hart-Becking spacing index

**\[deprecated\]**

Calculates the Hart Index or the Hart-Becking Index for even-aged stands

## Usage

``` r
silv_spacing_index(h0, ntrees, which = "hart")
```

## Arguments

- h0:

  Numeric vector with dominant height

- ntrees:

  Numeric vector with number of trees of the dominant height per hectare

- which:

  A character with the name of the index (either `hart` or
  `hart-brecking`). See details

## Value

A numeric vector

## Details

The spacing index can be used to determine whether a thinning is needed
or not, and also to determine how intense it should be.

- **Hart Index**: it assumes even-aged stands with square planting
  pattern.

- **Hart-Brecking Index**: it assumes triangular planting pattern.

## References

Assmann, E. (1970) The principles of forest yield study: Studies in the
organic production, structure, increment, and yield of forest stands.
Pergamon Press, Oxford.

## Examples

``` r
library(dplyr)
## Calculate spacing index for each plot
inventory_samples |>
  summarise(
    h0     = silv_dominant_height(diameter, height),
    ntrees = n(),
    .by    = plot_id
  ) |>
  ## calculate number of trees per hectare
  mutate(ntrees_ha = silv_ntrees_ha(ntrees, plot_size = 14.1)) |>
  mutate(spacing = silv_spacing_index(h0, ntrees_ha))
#> Warning: There was 1 warning in `mutate()`.
#> ℹ In argument: `spacing = silv_spacing_index(h0, ntrees_ha)`.
#> Caused by warning:
#> ! `silv_spacing_index()` was deprecated in silviculture 0.2.0.
#> ℹ Function `silv_spacing_index() is deprecated in favour of
#>   `silv_density_hart()`, and it will be removed in the next release.
#> # A tibble: 5 × 5
#>   plot_id    h0 ntrees ntrees_ha spacing
#>     <int> <dbl>  <int>     <dbl>   <dbl>
#> 1       7 17.4      16      256.    35.9
#> 2       8  7.21     22      352.    73.9
#> 3      10 13.0      25      400.    38.4
#> 4      53 21.3      19      304.    26.9
#> 5     189 11.0      80     1281.    25.4
```
