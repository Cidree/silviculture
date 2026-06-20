# List species supported by SNFI volume equations

Returns a data frame of species codes and names from the SNFI4 species
catalogue.

## Usage

``` r
silv_snfi_species(snfi_version = "SNFI4")
```

## Arguments

- snfi_version:

  Character. Either `"SNFI4"` (default) or `"SNFI3"`.

## Value

A `data.frame` with columns `species_code` and `species_name`.

## Examples

``` r
head(silv_snfi_species())
#>   species_code          species_name
#> 1            1 Heberdenia bahamensis
#> 2            7           Acacia spp.
#> 3            8   Phillyrea latifolia
#> 4           11   Ailanthus altissima
#> 5           12      Malus sylvestris
#> 6           13      Celtis australis
head(silv_snfi_species("SNFI3"))
#>   species_code          species_name
#> 1            1 Heberdenia bahamensis
#> 2            2    Amelanchier ovalis
#> 3            3        Frangula alnus
#> 4            4     Rhamnus alaternus
#> 5            5    Euonymus europaeus
#> 6            6       Myrtus communis
```
