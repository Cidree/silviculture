# Estimates tree height (m) from DBH (cm)

Estimates total tree height (m) using height-diameter (h-d) equations.
Currently, only models developed for Spain are available.

## Usage

``` r
silv_predict_height(
  diameter,
  model = eq_hd_vazquez_veloso_2025("All the species"),
  quiet = FALSE
)
```

## Arguments

- diameter:

  Numeric vector with diameters in cm (DBH).

- model:

  A ModelHD object. An object configured via the `eq_hd_*()` family of
  functions. Defaults to `eq_hd_vazquez_veloso_2025("All the species")`.

- quiet:

  A logical value. If TRUE, suppresses any informational messages.

## Value

A numeric vector with predicted heights (m).

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
# 1. Predict height using the default model (Vázquez-Veloso 2025 generic model)
predicted_heights_default <- silv_predict_height(diameter = c(20, 25, 30))
#> ! Cite this model using <https://doi.org/10.1016/j.foreco.2025.122981>
print(predicted_heights_default)
#> [1] 14.07234 15.90694 17.50925

# 2. Load the S7 ModelHD object for Pinus pinaster
model <- eq_hd_vazquez_veloso_2025("Pinus pinaster")

# 3. Vector-based calculation: predict tree heights from diameters
diameters <- c(20, 25, 30)
predicted_heights <- silv_predict_height(diameter = diameters, model = model)
#> ! Cite this model using <https://doi.org/10.1016/j.foreco.2025.122981>
print(predicted_heights)
#> [1]  9.494761 11.123536 12.606415

# 4. Dataset-based tutorial: apply to a forest inventory data frame
inventory <- data.frame(
  tree_id = 1:3,
  species = c("Pinus pinaster", "Pinus pinaster", "Pinus pinaster"),
  dbh_cm  = c(18.5, 22.1, 29.4)
)

# Apply prediction and append a new column to the dataset
inventory$height_m <- silv_predict_height(
  diameter = inventory$dbh_cm,
  model    = model
)
#> ! Cite this model using <https://doi.org/10.1016/j.foreco.2025.122981>
print(inventory)
#>   tree_id        species dbh_cm  height_m
#> 1       1 Pinus pinaster   18.5  8.972205
#> 2       2 Pinus pinaster   22.1 10.198817
#> 3       3 Pinus pinaster   29.4 12.435168
```
