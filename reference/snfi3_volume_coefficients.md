# SNFI3 tree volume coefficients

Allometric equation coefficients for tree volume and increment from the
3rd Spanish National Forest Inventory (SNFI3/IFN3), as documented by
MITECO.

## Usage

``` r
snfi3_volume_coefficients
```

## Format

A `tibble` with 37,730 rows and 16 variables:

- Codigo_provincia:

  Integer. Province numeric code.

- Nombre_provincia:

  Character. Province name.

- Codigo_especie:

  Integer. Species numeric code.

- Especie:

  Character. Species scientific name.

- Parametro:

  Character. Volume component: VCC (merchantable volume with bark), VSC
  (merchantable volume without bark), IAVC (annual volume increment with
  bark), or VLE (coarse firewood volume).

- F.c.:

  Integer. Tree quality class (forma de copa), from 1 (healthy and
  straight tree) to 6 (dead tree).

- Par_esp:

  Character. Special parameter metadata.

- Modelo:

  Integer. Equation model index.

- a, b, c, d, p, q, r:

  Numeric. Equation coefficients (NA where not applicable).

- R2:

  Numeric. Coefficient of determination of the model.

## References

MITECO. 3rd Spanish National Forest Inventory - SIG database codes.
<https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/documentador_bdcampo_ifn3_tcm30-282240.pdf>
