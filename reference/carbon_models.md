# Carbon content models

Carbon content percentages per biomass component available in the
`silviculture` package, compiled from Díeguez-Aranda et al. (2009) and
Montero et al. (2005). If you would like to suggest new models, please
open a new issue.

## Usage

``` r
carbon_models
```

## Format

A `tibble` with 264 rows and 13 variables:

- article_id:

  Character. Short identifier of the source article (e.g.
  `"montero-2005"`, `"dieguez-aranda-2009"`).

- title:

  Character. Full title of the source article.

- doi_url:

  Character. DOI URL of the source article.

- country:

  Character. Country where the study was conducted.

- region:

  Character. Region within the country.

- species:

  Character. Scientific name of the tree species.

- biomass_group:

  Character. Biomass group (`"AGB"` or `"BGB"`).

- tree_group:

  Character. Sub-group within the biomass group.

- tree_component:

  Character. Tree component (e.g. `"stem"`, `"bark"`,
  `"thick branches"`, `"small branches"`, `"twigs"`, `"leaves"`,
  `"needles"`, `"roots"`).

- carbon_percentage:

  Numeric. Carbon content as a percentage (0-100). Multiply by the
  component biomass and divide by 100 to obtain carbon mass in the same
  units. `NA` where not reported in the source.

- r2:

  Numeric. Coefficient of determination of the original model, if
  reported.

- rmse:

  Numeric. Root mean square error of the original model, if reported.

- obs:

  Character. Additional observations from the source article.

## References

Díeguez-Aranda, U., Rojo Alboreca, A., Castedo-Dorado, F., Álvarez
González, J.G., Barrio-Anta, M., Crecente-Campo, F., González González,
J.M., Pérez-Cruzado, C., Rodríguez Soalleiro, R., López-Sánchez, C.A.,
Balboa-Murias, M.A., Gorgoso Varela, J.J., Sánchez Rodríguez, F. (2009).
*Herramientas selvícolas para la gestión forestal sostenible en
Galicia*. Xunta de Galicia.

Montero, G., Ruiz-Peinado, R., Muñoz, M. (2005). *Producción de biomasa
y fijación de CO2 por los bosques españoles*. Monografías INIA: Serie
Forestal n.º 13. Instituto Nacional de Investigación y Tecnología
Agraria y Alimentaria, Madrid.
