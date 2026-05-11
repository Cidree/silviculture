# Estimates tree height from DBH

This function is intended to be used in
[`silv_predict_height()`](https://cidree.github.io/silviculture/reference/silv_predict_height.md).
It implements the h-d equations developed in Vázquez-Veloso et al.
(2025). These equations have been developed using the Spanish National
Forest Inventory, and therefore, they should only be applied within
Spain. The model includes parameters for 91 tree species.

## Usage

``` r
eq_hd_vazquez_veloso_2025(
  species,
  bioregion = "mediterranean",
  origin = "natural",
  mixture = "pure"
)
```

## Arguments

- species:

  A character string specifying the scientific name of the tree species.
  It can be a column name if all the species are included in this model.
  See Details for available species. If not specified, it takes the
  value "All the species", which corresponds to a generic model
  applicable to all species.

- bioregion:

  The biogeopgrahic region of the species. Available options are:
  `mediterranean`, `atlantic`, `alpine`, and `macaronesian`. If not
  specified, it takes the value `mediterranean`, which is the most
  common region in Spain. You can check the distribution of regions
  here:
  https://ars.els-cdn.com/content/image/1-s2.0-S037811272500489X-gr1.jpg

- origin:

  The origin of the stand. Available options are: `natural` and
  `plantation`. If not specified, it takes the value `natural`, which is
  the most common origin in Spain.

- mixture:

  The species available in the stand. Available options are: `pure` and
  `mix`. Consider the characteristics of the plot you are evaluating and
  not the entire forest, as the conditions of each stand are different.
  In this study, it was considered a stand to be mixed when the combined
  proportion of at least two species exceeds 90% of the plot's basal
  area, and the proportion of both species is greater than 15% of the
  total. It does not matter which species is accompanying or the
  proportion of mixing. If not specified, it takes the value `pure`,
  which is the most common condition in Spain.

## Value

A numeric vector with predicted height

## Details

The model adjusts the species-specific coefficients using the selected
bioregion, stand origin, and mixture type before returning the model
object.

## References

Vázquez-Veloso, A., Yang, S.-I., Bullock, B.P., Bravo, F., 2025. One
model to rule them all: A nationwide height–diameter model for 91
Spanish forest species. Forest Ecology and Management 595, 122981.
https://doi.org/10.1016/j.foreco.2025.122981

## See also

[`silv_predict_height()`](https://cidree.github.io/silviculture/reference/silv_predict_height.md)

## Examples

``` r
model <- eq_hd_vazquez_veloso_2025("All the species")
silv_predict_height(25, model)
#> ! Cite this model using <https://doi.org/10.1016/j.foreco.2025.122981>
#> [1] 15.90694
```
