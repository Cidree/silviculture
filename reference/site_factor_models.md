# Site Factor models

Coefficients for calculating the Site Factor (SF) based on Aguirre et
al. (2022). The site factor is a variable that can be used in growth
equations and is based on the dominant height and dominant dbh.

## Usage

``` r
site_factor_models
```

## Format

A `tibble` with 24 rows and 7 variables:

- species:

  Character. Scientific name of the tree species.

- species_code:

  Numeric. Species numeric code.

- d_ref:

  Numeric. Reference diameter (Dref).

- model:

  Character. Model shape and expanded parameter.

- param_a:

  Numeric. Parameter a.

- param_b:

  Numeric. Parameter b.

- param_c:

  Numeric. Parameter c.

## References

Aguirre, A. et al. (2022).
