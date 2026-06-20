# SNFI4 tree volume coefficients

Allometric equation coefficients for tree volume and increment from the
4th Spanish National Forest Inventory (SNFI4/IFN4), as documented by
MITECO.

## Usage

``` r
snfi4_volume_coefficients
```

## Format

A `tibble` with 20,925 rows and 15 variables:

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

  Integer. Tree quality class (calidad del árbol), from 1 (healthy and
  straight tree) to 6 (dead tree).

- Modelo:

  Integer. Equation model index.

- a, b, c, d, p, q, r:

  Numeric. Equation coefficients (NA where not applicable).

- D.n.m.:

  Character. Mean plot diameter metadata (diámetro normal medio),
  required for IAVC equation model 13.

## References

MITECO. 4th Spanish National Forest Inventory - SIG database codes.
<https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/documentador_sig_tcm30-536622.pdf>
