# Calculate Basal Area Larger (BAL)

Calculates the Basal Area Larger (BAL) for each tree in each plot.

## Usage

``` r
silv_tree_bal(
  data,
  plot_id,
  tree_id,
  diameter = NULL,
  expansion_factor = NULL,
  basal_area_ha = NULL,
  units = "cm"
)
```

## Arguments

- data:

  A data frame containing tree measurements.

- plot_id:

  Unquoted column name with the plot identifier.

- tree_id:

  Unquoted column name with the tree identifier.

- diameter:

  Unquoted column name with the DBH (optional).

- expansion_factor:

  Unquoted column name with the expansion factor (optional).

- basal_area_ha:

  Unquoted column name with the basal area per hectare. If provided,
  `diameter` and `expansion_factor` are ignored.

- units:

  The units of the diameter.

## Value

A numeric vector with the BAL for each tree.

## Examples

``` r
if (FALSE) { # \dontrun{
inventory_samples |>
  dplyr::mutate(bal = silv_tree_bal(
    inventory_samples, plot_id, tree_id, diameter, exp_factor))
} # }
```
