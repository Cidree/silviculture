# Calculates sample size for a random sampling inventory

**\[deprecated\]**

## Usage

``` r
silv_sample_size(
  x,
  plot_size,
  total_area,
  method = "random",
  max_error = 0.05,
  conf_level = 0.95,
  max_iter = 1000,
  quiet = FALSE
)
```

## Arguments

- x:

  vector of field survey

- plot_size:

  a numeric vector of length one with plot size in squared meters

- total_area:

  total area of the study area in squared meters

- method:

  sampling method. Available options are `random`

- max_error:

  maximum allowed error

- conf_level:

  confidence level

- max_iter:

  maximum number of iteration to find the plot size

- quiet:

  if `TRUE`, messages will be supressed

## Value

SampleSize object

## Examples

``` r
## pilot inventory measuring 4 plots of 25x25 meters
## total forest area 15 ha
## measured variable (x): basal area per hectare
silv_sample_size(
  x          = c(33, 37.5, 42, 35.2),
  plot_size  = 25 * 25,  # squared plot of 25x25
  total_area = 15 * 1e4, # 15 ha
  max_error  = 0.05,
  conf_level = 0.95,
  max_iter   = 100
)
#> Warning: `silv_sample_size()` was deprecated in silviculture 0.2.0.
#> ℹ Function `silv_sample_size() is deprecated in favour of
#>   `silv_sample_size_simple()`, and it will be removed in the next release.
#> ℹ A total of 4 plots were measured in the pilot inventory, each plot measuring 625 squared meters.
#> ℹ A minimum of 18 inventory plots are needed for a maximum sampling error of 5% (95% CI [35.08, 38.77]).
#> ℹ The sampling effort will be 1.2 plots/ha
#> ℹ Note that these calculations assume that you will do a simple random sampling
#> <silviculture::SampleSize>
#>  @ sampling_res :List of 4
#>  .. $ min_plots      : num 18
#>  .. $ ci_lo          : num 35.1
#>  .. $ ci_up          : num 38.8
#>  .. $ sampling_effort: num 1.2
#>  @ sampling_opts:List of 5
#>  .. $ pilot_plots: num [1:4] 33 37.5 42 35.2
#>  .. $ plot_size  : num 625
#>  .. $ total_area : num 150000
#>  .. $ max_error  : num 0.05
#>  .. $ conf_level : num 0.95
```
