# List provinces supported by SNFI volume equations

Returns a data frame with province codes, names, and which SNFI dataset
(SNFI4 or SNFI3 fallback) is used for each.

## Usage

``` r
silv_snfi_provinces()
```

## Value

A `data.frame` with columns `province_id`, `province_name`, and
`snfi_version`.

## Examples

``` r
silv_snfi_provinces()
#>    province_id          province_name snfi_version
#> 1            4                Almería        SNFI3
#> 2           11                  Cádiz        SNFI3
#> 3           14                Córdoba        SNFI3
#> 4           18                Granada        SNFI3
#> 5           21                 Huelva        SNFI3
#> 6           23                   Jaén        SNFI3
#> 7           29                 Málaga        SNFI3
#> 8           41                Sevilla        SNFI3
#> 9           22                 Huesca        SNFI4
#> 10          44                 Teruel        SNFI4
#> 11          50               Zaragoza        SNFI4
#> 12          33               Asturias        SNFI4
#> 13           7         Balears, Illes        SNFI4
#> 14          35            Palmas, Las        SNFI4
#> 15          38 Santa Cruz de Tenerife        SNFI4
#> 16          39              Cantabria        SNFI4
#> 17           5                  Ávila        SNFI4
#> 18           9                 Burgos        SNFI4
#> 19          24                   León        SNFI4
#> 20          34               Palencia        SNFI4
#> 21          37              Salamanca        SNFI4
#> 22          40                Segovia        SNFI4
#> 23          42                  Soria        SNFI4
#> 24          47             Valladolid        SNFI4
#> 25          49                 Zamora        SNFI4
#> 26           2               Albacete        SNFI4
#> 27          13            Ciudad Real        SNFI4
#> 28          16                 Cuenca        SNFI4
#> 29          19            Guadalajara        SNFI4
#> 30          45                 Toledo        SNFI4
#> 31           8              Barcelona        SNFI4
#> 32          17                 Girona        SNFI4
#> 33          25                 Lleida        SNFI4
#> 34          43              Tarragona        SNFI4
#> 35           3       Alicante/Alacant        SNFI3
#> 36          12     Castellón/Castelló        SNFI3
#> 37          46      Valencia/València        SNFI3
#> 38           6                Badajoz        SNFI4
#> 39          10                Cáceres        SNFI4
#> 40          15              Coruña, A        SNFI4
#> 41          27                   Lugo        SNFI4
#> 42          32                Ourense        SNFI4
#> 43          36             Pontevedra        SNFI4
#> 44          28                 Madrid        SNFI4
#> 45          30                 Murcia        SNFI4
#> 46          31                Navarra        SNFI4
#> 47           1            Araba/Álava        SNFI4
#> 48          48                Bizkaia        SNFI4
#> 49          20               Gipuzkoa        SNFI4
#> 50          26              Rioja, La        SNFI4
#> 51          51                  Ceuta        SNFI3
#> 52          52                Melilla        SNFI3
```
