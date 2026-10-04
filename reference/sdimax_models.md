# Maximum stand density index (SDImax) models

Coefficients for calculating maximum stand density index (SDImax) from
Rodríguez de Prado (2020).

## Usage

``` r
sdimax_models
```

## Format

A `tibble` with 88 rows and 13 variables:

- article_id:

  Character. Identifier of the article.

- title:

  Character. Title of the article.

- doi_url:

  Character. DOI URL of the article.

- country:

  Character. Country where the study was conducted.

- species:

  Character. Tree species scientific name.

- model_name:

  Character. Name of the model/equation variant (e.g. "basic", "P1",
  "MXT3").

- a0:

  Numeric. Coeffient a0.

- a1:

  Numeric. Coeffient a1 (0 if not used/applicable).

- b0:

  Numeric. Coeffient b0.

- b1:

  Numeric. Coeffient b1 (0 if not used/applicable).

- aic:

  Numeric. Akaike Information Criterion.

- pseudo_r2:

  Numeric. Pseudo R-squared value.

- q_index:

  Numeric. Q index value.

## References

Rodríguez-de-Prado, M., et al. (2020). Potential climatic influence on
maximum stand carrying capacity for 15 Mediterranean coniferous and
broadleaf species. Forest Ecology and Management, 458, 117824.
