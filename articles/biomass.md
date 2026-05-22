# Biomass Models

``` r

library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(silviculture)
#> 
#> Attaching package: 'silviculture'
#> The following object is masked from 'package:graphics':
#> 
#>     plot
#> The following object is masked from 'package:base':
#> 
#>     plot
```

## Introduction

## The ModelBiomass object

Biomass models are bundled around the `S7` object-oriented system. We
tried to design the models to be flexible, easy to use, and extendible.
The functions to create a biomass model begin with `eq_biomass_`,
continued by the identifier of the models, which is typically the first
author and the year of the article where the study of the models can be
found. Each function refer to an article that is typically focused in a
bunch of tree species, or in a specific area. For instance,
[`eq_biomass_ruiz_peinado_2011()`](https://cidree.github.io/silviculture/reference/eq_biomass_ruiz_peinado_2011.md)
includes biomass models for Spanish softwood species (e.g. *Pinus
pinaster*, *Pinus nigra*…). Depending on the model we will find
different arguments:

- `species`: present in all the models. The Latin name of the species.
  The functions are designed to be flexible, so it can be calculated for
  one species, or for a vector of species (i.e. we can easily add the
  biomass component to a `data.frame` using a
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  statement).

- `component`: the component is probably the most challenging argument,
  since in each study the different parts of the tree can be named
  slightly different. However, we tried to create a standarized
  architecture of these arguments. The user can specify main components,
  or single components. A good way to check the available components for
  a specific model (note that the components might not be available for
  all the species):

``` r

biomass_models |> 
  filter(article_id == "ruiz-peinado-2011") |> 
  count(biomass_group, tree_group, tree_component)
#> # A tibble: 7 × 4
#>   biomass_group tree_group tree_component                n
#>   <chr>         <chr>      <chr>                     <int>
#> 1 AGB           branches   medium branches               7
#> 2 AGB           branches   small branches and leaves    10
#> 3 AGB           branches   thick and medium branches     3
#> 4 AGB           branches   thick branches                7
#> 5 AGB           stem       stem                         10
#> 6 ALL           all        tree                         10
#> 7 BGB           roots      roots                         9
```

The `component` argument is flexible, and the user can either introduce
a biomass group, a tree group, or a tree component.

- `return_rmse` or `return_r2`: whether to return the error of the model
  instead of the biomass value. This only works for some tree
  components, and it’s not always available.

Now, how does the ModelBiomass look like? Imagine we will calculate the
aboveground biomass of three trees. Two of them are *Pinus pinaster*,
and the third one is a *Pinus sylvestris*:

``` r

eq_biomass_ruiz_peinado_2011(
  c("Pinus pinaster", "Pinus sylvestris", "Pinus pinaser"),
  component = "AGB"
)
#> <silviculture::ModelBiomass>
#>  @ equation  : chr "ruiz-peinado-2011"
#>  @ species   : chr [1:3] "Pinus pinaster" "Pinus sylvestris" "Pinus pinaser"
#>  @ component : chr "AGB"
#>  @ expression:'data.frame':  7 obs. of  2 variables:
#>  .. $ expression: chr  "0.0278 * d^2.115 * h^0.618" "0.000381 * d^3.141" "0.0129 * d^2.320" "0.0154 * d^2 * h" ...
#>  .. $ species   : chr  "Pinus pinaster" "Pinus pinaster" "Pinus pinaster" "Pinus sylvestris" ...
#>  @ url       : chr "https://doi.org/10.5424/fs/2011201-11643"
#>  @ obs       : chr "Diameter is assumed to in centimeters, and height is assumed to be in meters"
#>  @ params    :List of 3
#>  .. $ return_rmse: logi FALSE
#>  .. $ comp       : chr [1:7] "stem" "thick and medium branches" "small branches and leaves" "stem" ...
#>  .. $ rmse       : num [1:7] 14.47 7.04 7.67 34.01 12.63 ...
```

The S7 object stores several data that will lately be used by
[`silv_predict_biomass()`](https://cidree.github.io/silviculture/reference/silv_predict_biomass.md),
but now let’s try to understand what this object is:

- `equation`: identifier of the model.

- `species`: the three trees we inserted as input of the model.

- `component`: the biomass component (AGB = AboveGround Biomass).

- `expression`: a data frame with two columns:

  - `expression`: all the necessary expressions to calculate the
    selected component. For the AGB we need the biomass equations of the
    stem, and branches.

  - `species`: the species of the expression.

- `url`: link to the article where the source of the biomass models can
  be found.

- `obs`: observations.

- `params`: extra parameters that depend on the model. We can see for
  instance that for *Pinus pinaster* we are summing the biomass of the
  stem; thick and medium branches; small branches and leaves.

This function on its own is useless. We need to introduce it into
[`silv_predict_biomass()`](https://cidree.github.io/silviculture/reference/silv_predict_biomass.md),
which is the main function for calculating the biomass. This functions
takes a couple of main arguments that are necessary is most of the
biomass models:

- `diameter`: diameter of the tree in cm

- `height`: height of the tree in cm. The height is needed in most of
  the models, although there are some that use only the diameter.

- `model`: here we introduce the `eq_biomass_*()` function.

- `ntrees`: number of trees with the specified diameter and height.

Let’s calculate the AGB of the three trees we saw before, but we will
structure them in a data frame:

``` r

trees_tbl <- data.frame(
  sp   = c("Pinus pinaster", "Pinus sylvestris", "Pinus pinaster"),
  dbh  = c(40, 42, 38),
  h    = c(23, 21, 25.5)
)
```

The
[`silv_predict_biomass()`](https://cidree.github.io/silviculture/reference/silv_predict_biomass.md)
is a vectorized function, so we can add the biomass as a new column
using the
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
function:

``` r

trees_tbl |> 
  mutate(
    agb = silv_predict_biomass(
      diameter = dbh,
      height   = h,
      model    = eq_biomass_ruiz_peinado_2011(
        species   = sp,
        component = "AGB"
      )
    )
  )
#> ! Cite this model using <https://doi.org/10.5424/fs/2011201-11643>
#> ℹ Diameter is assumed to in centimeters, and height is assumed to be in meters
#>                 sp dbh    h      agb
#> 1   Pinus pinaster  40 23.0 580.2280
#> 2 Pinus sylvestris  42 21.0 689.1023
#> 3   Pinus pinaster  38 25.5 545.9428
```
