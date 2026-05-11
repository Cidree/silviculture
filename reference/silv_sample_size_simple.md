# Calculates sample size for a simple random sampling (SRS)

Calculates the sample size needed for a SRS inventory, estimated from
pilot inventory data.

## Usage

``` r
silv_sample_size_simple(
  x,
  plot_size,
  total_area,
  max_error = 0.05,
  conf_level = 0.95,
  max_iter = 1000,
  quiet = FALSE
)
```

## Arguments

- x:

  vector of the variable measured in the pilot inventory (e.g. basal
  area, volume)

- plot_size:

  a numeric vector of length one with plot size in squared meters

- total_area:

  total area of the study area in squared meters

- max_error:

  maximum allowed relative error

- conf_level:

  confidence level

- max_iter:

  maximum number of iteration to find the plot size

- quiet:

  if `TRUE`, messages will be supressed

## Value

SimpleSampleSize object

## Details

Sample size is very important to be optimized, since a small sample size
will entail a higher error, while a huge sample size will entail higher
costs. The SRS is typically used for random sampling, although it might
be used also for regular sampling. The number of samples is calculated
using the expression:

\\n \ge \frac{t^2 \cdot CV^2}{\epsilon^2 + \frac{t^2 \cdot CV^2}{N}}\\

Where:

- **t**: the value of student's t for given sample size of the pilot
  inventory

- **CV**: the coefficient of variation of `x`

- \\\epsilon\\: the relative error (`max_error`)

- **N**: the size of the pilot inventory

`x` is a variable measured in a pilot inventory. Let's say we measure
forest variables in 10 pilot plots, aiming at basal area measurement so
we have to measure only the DBH. After some calculations, we will have
the basal area per hectare in each of the 10 plots. The sample size is
then calculated from the variation of these values and the error that we
will allow.

## Examples

``` r
## pilot inventory measuring 4 plots of 25x25 meters
## total forest area 15 ha
## measured variable (x): basal area per hectare
silv_sample_size_simple(
  x          = c(33, 37.5, 42, 35.2),
  plot_size  = 25 * 25,  # squared plot of 25x25
  total_area = 15 * 1e4, # 15 ha
  max_error  = 0.05,
  conf_level = 0.95,
  max_iter   = 100
)
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
