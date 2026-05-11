# Calculates sample size for a stratified sampling

Calculates the sample size needed for a stratified inventory, estimated
from pilot inventory data.

## Usage

``` r
silv_sample_size_stratified(
  data,
  x,
  strata,
  total_area,
  plot_size,
  method = "optimal",
  cost = NA,
  max_error = 0.05,
  conf_level = 0.95,
  max_iter = 1000,
  currency = "EUR",
  quiet = FALSE
)
```

## Arguments

- data:

  a `data.frame` of pilot inventory data

- x:

  name of the variable in `data` that was measured (e.g. basal area,
  volume)

- strata:

  name of the variable in `data` with the name of the stratum

- total_area:

  name of the variable in `data` with the area of the stratum

- plot_size:

  a numeric vector of length one with plot size in squared meters

- method:

  a charater vector of length one with the id of the method. Available
  options are `optimal`, `cost`, and `prop`. See details

- cost:

  name of the variable in `data` with the average cost of measuring one
  plot of the stratum. Used with `method = 'cost'` for sample size, and
  for message output in other methods

- max_error:

  maximum allowed relative error

- conf_level:

  confidence level

- max_iter:

  maximum number of iteration to find the plot size

- currency:

  currency to be shown in console output when using `method = 'cost'`

- quiet:

  if `TRUE`, messages will be supressed

## Value

S7 `StratifiedSampleSize` object with:

- **results**: `data.frame` with the main results by stratum

- **strata_error**: `data.frame` with maximum absolute error \\\mp\\ C.I
  (max_abs_error, x_min, x_max), and the esimator of the typical error
  \\\mp\\ C.I (sampling error, x_ci_lo, x_ci_hi)

- **sampling_error**: `data.frame` with the maximum absolute error
  \\\mp\\ C.I (max_abs_error, x_min, x_max), and the typical sampling
  error of the weighted mean \\\mp\\ C.I (sampling error, x_ci_lo,
  x_ci_hi)

- **sampling_opts**: `list` with function options

## Details

Stratified Sampling calculates the number of plots to be inventored in
different strata. For instance, you might have *Pinus sylvestris* and
*Pinus pinaster* plots in the same forest, and you might want to get the
optimal number of plots for field inventory of each stratum, for a given
maximum relative error (e.g. 5%), and with a certain level of confidence
(e.g 95%). Of course, the area of *P. sylvestris* will be different than
the area occupied by *P. pinaster*. For instance, the total area of *P.
sylvestris* could be 100 ha, while the area of *P. pinaster* could be
200 ha. Therefore, you need to create a pilot inventory and measure a
variable such as basal area maybe in 5 pilot plots of *P. sylvestris*
and 7 pilot plots of *P. pinaster*. With that data collected, you can
use three stratified sample size methods:

- **Optimal Allocation with Constant Cost**: using `method = 'optimal'`.
  The sampling units are distributed within the different strata taking
  into account the size (e.g. 100 ha vs 200 ha) and the heterogeinity
  (e.g. differences in basal area). It minimizes the number of sampling
  units.

\\n = \frac{t^2\_{n - m} \cdot (\sum^{j = m}\_{j = 1} P_j \cdot s_j)^2
}{\epsilon^2 + \frac{t^2\_{n - m} \cdot \sum^{j = m}\_{j = 1} P_j \cdot
s_j^2}{N}}\\

- **Optimal Allocation with Variable Cost**: using `method = 'cost'`.
  This method needs to know the cost of a sampling unit in each strata.
  It will minimize the cost of the inventory, taking into account the
  size, the heterogeinity, and the cost of the sampling unit of the
  strata.

\\n = \frac{t^2\_{n-m} \cdot (\sum^{j = m}\_{j = 1} \cdot P_j \cdot s_j
\cdot \sqrt{c_j}) \cdot (\sum^{j = m}\_{j = 1} \cdot \frac{P_j \cdot
s_j}{\sqrt{c_j}})}{\epsilon^2 + \frac{t^2\_{n - m} \cdot \sum^{j =
m}\_{j = 1} P_j \cdot s_j^2}{N}}\\

- **Proportional Allocation**: using `method = 'prop'`. The sampling
  units are distributed proportional to the size of the strata. In the
  example, 33% of the estimated sampling units will be allocated to *P.
  sylvestris* and 66% to *P. pinaster*.

\\n = \frac{t^2\_{n - m} \cdot \sum^{j = m}\_{j = 1} P_j \cdot s_j^2
}{\epsilon^2 + \frac{t^2\_{n - m} \cdot \sum^{j = m}\_{j = 1} P_j \cdot
s_j^2}{N}}\\

Where:

- **n**: estimated sample size

- **t**: the value of student's t

- **\\P_j\\**: proportion of pilot plots of \\j^{th}\\ strata

- **\\s_j\\**: standard deviation of `x`

- **\\s_j^2\\**: variance of `x`

- **N**: population size (number of plots of `plot_size` that fit in
  `total_area`)

- **\\\epsilon\\**: maximum allowed absolute error. Calculated from `x`
  and `max_error`

- **N**: the size of the pilot inventory

## Examples

``` r
## read pilot inventory ficticious data
data_path <- system.file("extdata/pilot_inventory.csv", package = "silviculture")
inventory_tbl <- read.csv(data_path)

## calculate sample size
sample_size_list <- silv_sample_size_stratified(
  data  = inventory_tbl,
  x     = basal_area,
  strata = stratum,
  total_area = area,
  method = "optimal",
  cost = cost,
  plot_size = 100,
  conf_level = .95,
  max_error = .05
)
#> 
#> ── Pilot inventory ─────────────────────────────────────────────────────────────
#> You are estimating the sample size using a Stratified Sampling using the
#> Optimal Allocation with Constant Cost method.
#> • Pilot inventory: 30 plots
#> • Maximum allowed error: 5%
#> • Total sampling plots: 164
#> • Actual error: 4.97%
#> • Total cost of the inventory: 5725 EUR
#> 
#> ── Operational inventory ───────────────────────────────────────────────────────
#> 
#> ── Stratum - Pinus pinaster ──
#> 
#> • Pilot inventory with 11 plots, in 9 ha
#> • Minimum sampling plots: 46 with a relative error of 3.74% (95% CI [62.56,
#>   67.42])
#> • Sampling effort: 5.1 plots/ha
#> • Cost per hectare: 255 EUR/ha
#> • Total cost: 2300 EUR
#> 
#> 
#> ── Stratum - Pinus radiata ──
#> 
#> • Pilot inventory with 8 plots, in 12 ha
#> • Minimum sampling plots: 71 with a relative error of 4.12% (95% CI [52.85,
#>   57.39])
#> • Sampling effort: 5.9 plots/ha
#> • Cost per hectare: 206.5 EUR/ha
#> • Total cost: 2485 EUR
#> 
#> 
#> ── Stratum - Pinus sylvestris ──
#> 
#> • Pilot inventory with 11 plots, in 7 ha
#> • Minimum sampling plots: 47 with a relative error of 5.6% (95% CI [52.85,
#>   59.13])
#> • Sampling effort: 6.7 plots/ha
#> • Cost per hectare: 134 EUR/ha
#> • Total cost: 940 EUR
#> 
```
