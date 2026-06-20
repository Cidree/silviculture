# Predict tree volume using Spanish National Forest Inventory equations

Computes four volume metrics (in dm^3) for one or more trees using the
allometric equations from the 3rd and 4th Spanish National Forest
Inventory (SNFI3/SNFI4), as documented by MITECO (Anexo 19).

## Usage

``` r
silv_predict_snfi_volume(
  province,
  species,
  dbh = NULL,
  h = NULL,
  dnm = NULL,
  quality = "default",
  quiet = FALSE
)
```

## Arguments

- province:

  A character or numeric vector. Province name (e.g. `"Cantabria"`) or
  province code (e.g. `39`). See
  [`silv_snfi_provinces()`](https://cidree.github.io/silviculture/reference/silv_snfi_provinces.md).

- species:

  A character or numeric vector. Species scientific name (e.g.
  `"Pinus radiata"`) or SNFI species code (e.g. `28`). See
  [`silv_snfi_species()`](https://cidree.github.io/silviculture/reference/silv_snfi_species.md).

- dbh:

  A numeric vector of diameter at breast height in **cm**.

- h:

  A numeric vector of tree heights in **m**. Optional for some models
  (e.g. SNFI3 model 14), but required by most VCC equations.

- dnm:

  A numeric vector of mean plot diameter in **cm**. Required only when
  the IAVC equation uses model 13.

- quality:

  A character or numeric vector. Quality/shape class (`1`-`6`). Use
  `"default"` (the default) to select the first available class.

- quiet:

  A logical value. If `TRUE`, suppresses citation messages. Defaults to
  `FALSE`.

## Value

A `data.frame` with one row per input tree and five columns:

- `vcc`:

  Merchantable volume **with** bark (dm^3).

- `vsc`:

  Merchantable volume **without** bark (dm^3).

- `iavc`:

  Annual volume increment with bark (dm^3).

- `vle`:

  Coarse firewood volume (dm^3).

- `snfi_version`:

  Dataset used: `"SNFI4"` or `"SNFI3"`.

Any component for which parameters are missing or a required input is
`NA` returns `NA` for that column, without stopping execution.

## Details

### Model selection

For each province, SNFI4 data are preferred. When a province is not
covered by SNFI4 (11 provinces), the function falls back to SNFI3
automatically. SNFI3 also provides a supplementary file for species not
present in the standard provincial tables.

### Equation models

|           |                                       |
|-----------|---------------------------------------|
| Component | Models used                           |
| VCC       | 1, 11                                 |
| VSC       | 7                                     |
| IAVC      | 8, 13, 14, 16, 17, 18, 19, 20, 21, 25 |
| VLE       | 10, 12                                |

Full model equations are available in Anexo 19 of the MITECO SIG
documentation (see References).

### Units

Inputs: `dbh` and `dnm` in **cm** (converted to mm internally), `h` in
**m**. All outputs in **dm^3**.

## References

MITECO. 4th Spanish National Forest Inventory - SIG database codes.
<https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/documentador_sig_tcm30-536622.pdf>

MITECO. 3rd Spanish National Forest Inventory - SIG database codes.
<https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/documentador_bdcampo_ifn3_tcm30-282240.pdf>

## See also

[`silv_snfi_provinces()`](https://cidree.github.io/silviculture/reference/silv_snfi_provinces.md),
[`silv_snfi_species()`](https://cidree.github.io/silviculture/reference/silv_snfi_species.md),
[`silv_tree_volume()`](https://cidree.github.io/silviculture/reference/silv_tree_volume.md)

## Examples

``` r
# Single tree: Pinus radiata in Alava (province 1), code 28
silv_predict_snfi_volume(
  province = 1,
  species  = 28,
  dbh      = 20,
  h        = 15,
  dnm      = 23
)
#> ℹ SNFI4 data used. Cite: MITECO SNFI4 SIG codes - <https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/documentador_sig_tcm30-536622.pdf>
#>        vcc      vsc     iavc      vle snfi_version
#> 1 201.3054 146.1138 23.45224 13.60531        SNFI4

# Mixed inputs: province and species by name
silv_predict_snfi_volume(
  province = c(1, 39),
  species  = c("Pinus radiata", "Pinus radiata"),
  dbh      = c(20, 25),
  h        = c(15, 18),
  dnm      = c(23, 26)
)
#> ℹ SNFI4 data used. Cite: MITECO SNFI4 SIG codes - <https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/documentador_sig_tcm30-536622.pdf>
#>        vcc      vsc     iavc      vle snfi_version
#> 1 201.3054 146.1138 23.45224 13.60531        SNFI4
#> 2 342.0875 268.9089 20.02038 31.51750        SNFI4
```
