
#' Calculates Basal Area
#'
#' Calculates Basal Area in square meters.
#'
#' @template diameter
#' @template ntrees
#' @param units The units of the diameter (one of `mm`, `cm`, `dm`, or `m`)
#'
#' @return A numeric vector
#' @export
#'
#' @details
#' The function uses the next formula:
#'
#' \eqn{G = \frac{\pi}{40000} \cdot D^2 \cdot \text{ntrees}}
#'
#' where G is the basal area in \eqn{m^2}, and D is the diameter in `cm`. If ntrees 
#' in the number of trees per hectare, then the result will be \eqn{m^2/ha}. It is 
#' recommended to use the squared mean diameter calculated with [silv_stand_qmean_diameter()].
#' 
#' Note that if \code{ntrees = NULL}, the output of the function will be exactly
#' the same as in [silv_tree_basal_area()].
#'
#' @examples
#' ## calculate G for inventory data grouped by plot_id and species
#' library(dplyr)
#' inventory_samples |>
#' mutate(dclass = silv_tree_dclass(diameter)) |>
#'   summarise(
#'     height = mean(height, na.rm = TRUE),
#'     ntrees = n(),
#'     .by    = c(plot_id, species, dclass)
#'   ) |>
#'   mutate(
#'     ntrees_ha = silv_density_ntrees_ha(ntrees, plot_size = 10),
#'     dg        = silv_stand_qmean_diameter(dclass, ntrees_ha),
#'     g         = silv_stand_basal_area(dclass, ntrees_ha),
#'     .by       = c(plot_id, species)
#'   )
silv_stand_basal_area <- function(
  diameter,
  ntrees = NULL,
  units = "cm"
) {

  # 0. Validate inputs
  assert_positive_numeric(diameter, "diameter")
  if (is.numeric(ntrees) && length(ntrees) != length(diameter)) 
    cli::cli_abort("`ntrees` must have the same length as `diameter` or be NULL")

  ## If ntrees = NULL, only one tree assumed
  if (is.null(ntrees)) {
    ntrees <- rep(1, length(diameter))
  } else {
    assert_positive_numeric(ntrees, "ntrees")
  }

  # 1. Calculate basal area
  switch(units,
    "mm" = (pi / 4) * (diameter / 1000)**2 * ntrees,
    "cm" = (pi / 4) * (diameter / 100)**2 * ntrees,
    "dm" = (pi / 4) * (diameter / 10)**2 * ntrees,
    "m"  = (pi / 4) * diameter**2 * ntrees,
    cli::cli_abort("Invalid `units`. Use one of {.val {c('mm', 'cm', 'dm', 'm')}}")
  )

}





#' Calculates the dominant height
#'
#' Calculates the dominant height using the Assman equation or the Hart equation
#'
#' @template diameter
#' @template height
#' @template ntrees
#' @param which The method to calculate the dominant height (see details)
#'
#' @details
#' The dominant height \eqn{H_0} is the mean height of dominant trees, which is
#' less affected than overall mean height by thinning or other treatments.
#'
#' - \bold{Assman}: calculates the \eqn{H_0} as the mean height of the 100 thickest
#' trees per hectare
#'
#' - \bold{Hart}: calculates the \eqn{H_0} as the mean height of the 100 tallest
#' trees per hectare
#' 
#' When \code{ntrees = NULL}, the function will assume that each diameter and height
#' belongs to only one tree. If you have data aggregated by hectare, you'll use the
#' number of trees per hectare in this argument.
#'
#' @references Assmann, E. (1970) The principles of forest yield study: Studies in the
#' organic production, structure, increment, and yield of forest stands. Pergamon Press, Oxford.
#'
#' @return A numeric vector
#' @export
#' @include utils-not-exported.R
#'
#' @examples
#' ## calculate h0 for inventory data grouped by plot_id and species
#' library(dplyr)
#' inventory_samples |>
#' mutate(dclass = silv_tree_dclass(diameter)) |>
#'   summarise(
#'     height = mean(height, na.rm = TRUE),
#'     ntrees = n(),
#'     .by    = c(plot_id, species, dclass)
#'   ) |>
#'   mutate(
#'     ntrees_ha = silv_density_ntrees_ha(ntrees, plot_size = 10),
#'     h0        = silv_stand_dominant_height(dclass, height, ntrees_ha),
#'     .by       = c(plot_id, species)
#'   )
silv_stand_dominant_height <- function(
  diameter,
  height,
  ntrees = NULL,
  which = c("assman", "hart")
) {

  # 0. Validate inputs
  dh_method <- match.arg(which)
  assert_positive_numeric(diameter, "diameter")
  assert_positive_numeric(height, "height")


  # 1. Create a data frame with input variables
  if (is.null(ntrees)) {
    data <- data.frame(
      d  = diameter,
      h  = height,
      nt = 1
    )
  } else {
    ## Validate ntrees as numeric after cheking it's not a NULL
    assert_positive_numeric(ntrees, "ntrees")
    data <- data.frame(
      d  = diameter,
      h  = height,
      nt = ntrees
    )
  }

  # 2. Calculate dominant height
  if (tolower(dh_method) == "assman") {
    h0 <- data |>
      ## sort descending by diameter class
      dplyr::arrange(dplyr::desc(d)) |>
      dplyr::mutate(
        .cumtrees = cumsum(nt),
        .nmax     = which(.cumtrees >= 100)[1],
        .nmax     = if (is.na(.nmax[1])) which.max(.cumtrees) else .nmax,
        .do       = calc_dominant_metric(.nmax, nt, h)
      ) |>
      dplyr::pull(.do)
  } else {
    h0 <- data |>
      ## sort descending by height
      dplyr::arrange(dplyr::desc(h)) |>
      dplyr::mutate(
        .cumtrees = cumsum(nt),
        .nmax     = which(.cumtrees >= 100)[1],
        .nmax     = if (is.na(.nmax[1])) which.max(.cumtrees) else .nmax,
        .do       = calc_dominant_metric(.nmax, nt, h)
      ) |>
      dplyr::pull(.do)
  }


  # 3. If it's not vectorized, retrieve just one value
  if (is.null(ntrees)) h0[1] else h0

}





#' Calculates the dominant diameter
#'
#' Calculates the dominant diameter using Assman and Friedrich method, or
#' Weise method
#'
#' @template diameter
#' @template ntrees
#' @param which The method to calculate the dominant diameter (see details)
#'
#' @details
#' The dominant diameter \eqn{D_0} is the mean diameter of the 100 thickest trees per
#' hectare. Therefore, `diameter` and `ntrees` should be vectors of the same length.
#' 
#' - \bold{Assman}: calculates the \eqn{D_0} as the mean diameter of the 100 thickest
#' trees per hectare
#'
#' - \bold{Weise}: calculates the \eqn{D_0} as the quadratic mean diameter of the
#' 20% thickest trees per hectare
#'
#' @return A numeric vector
#' @export
#' @include utils-not-exported.R
#'
#' @examples
#' ## calculate d0 for inventory data grouped by plot_id and species
#' library(dplyr)
#' inventory_samples |>
#' mutate(dclass = silv_tree_dclass(diameter)) |>
#'   summarise(
#'     height = mean(height, na.rm = TRUE),
#'     ntrees = n(),
#'     .by    = c(plot_id, species, dclass)
#'   ) |>
#'   mutate(
#'     ntrees_ha = silv_density_ntrees_ha(ntrees, plot_size = 10),
#'     d0        = silv_stand_dominant_diameter(dclass, ntrees_ha),
#'     .by       = c(plot_id, species)
#'   )
silv_stand_dominant_diameter <- function(
  diameter,
  ntrees = NULL,
  which = c("assman", "weise")
) {

  # 0. Validate inputs
  dh_method <- match.arg(which)
  assert_positive_numeric(diameter, "diameter")


  # 1. Create a data frame with input variables
  if (is.null(ntrees)) {
    data <- data.frame(
      d  = diameter,
      nt = 1
    )
  } else {
    assert_positive_numeric(ntrees, "ntrees")
    data <- data.frame(
      d  = diameter,
      nt = ntrees
    )
  }

  # 2. Calculate dominant diameter
  if (tolower(dh_method) == "assman") {
    d0 <- data |> 
        ## sort descending by diameter class
        dplyr::arrange(dplyr::desc(d)) |>
        dplyr::mutate(
          .cumtrees = cumsum(nt),
          .nmax     = which(.cumtrees >= 100)[1],
          .nmax     = if (is.na(.nmax[1])) which.max(.cumtrees) else .nmax,
          .do       = calc_dominant_metric(.nmax, nt, d)
        ) |>
        dplyr::pull(.do)
  } else {
    n_tickest_trees <- 0.2 * sum(data$nt)
    d0 <- data |> 
      ## sort descending by diameter class
      dplyr::arrange(dplyr::desc(d)) |>
      dplyr::mutate(
        .cumtrees = cumsum(nt),
        nt_sel = calc_accumulated_trees(nt, .cumtrees, n_tickest_trees),
        .do = silv_stand_qmean_diameter(d, nt_sel)
      ) |> 
      dplyr::pull(.do)
  }


  # 3. If it's not vectorized, retrieve just one value
  if (is.null(ntrees)) d0[1] else d0

}





#' Calculates Lorey's Height
#'
#' Tree's mean height weighted by basal area
#'
#' @template height
#' @template g
#' @template ntrees
#'
#' @return A numeric vector
#' @export
#'
#' @details
#' The function calculates Lorey's mean height according to:
#'
#' \deqn{h_L = \frac{\sum n_i g_i h_i}{\sum n_i g_i}}
#'
#' When ntrees is not provided (i.e. \code{ntrees = NULL}) the formula is simply
#' the weighted mean of the provided heights and basal areas:
#'
#' \deqn{h_L = \frac{\sum g_i h_i}{\sum g_i}}
#'
#' @examples
#' ## Calculate Lorey's Height by plot and species
#' library(dplyr)
#' inventory_samples |>
#'   mutate(g = silv_tree_basal_area(diameter)) |>
#'   summarise(
#'     lh  = silv_stand_lorey_height(height, g),
#'     .by = c(plot_id, species)
#'   )
silv_stand_lorey_height <- function(
  height, 
  g, 
  ntrees = NULL
) {

  # 0. Handle errors
  assert_same_length(height, g, names = c("height", "g"))

  # 1. Calculate
  if (is.null(ntrees)) {
    weighted.mean(height, g)
  } else {
    assert_positive_numeric(ntrees, "ntrees")
    assert_same_length(height, ntrees, names = c("height", "ntrees"))
    weighted.mean(height, g * ntrees)
  }

}





#' Calculates the quadratic mean diameter (QMD)
#'
#' @template diameter
#' @template ntrees
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' ## calculate dg for inventory data grouped by plot_id and species
#' library(dplyr)
#' inventory_samples |>
#' mutate(dclass = silv_tree_dclass(diameter)) |>
#'   summarise(
#'     height = mean(height, na.rm = TRUE),
#'     ntrees = n(),
#'     .by    = c(plot_id, species, dclass)
#'   ) |>
#'   mutate(
#'     ntrees_ha = silv_density_ntrees_ha(ntrees, plot_size = 10),
#'     h0        = silv_stand_dominant_height(dclass, height, ntrees_ha),
#'     dg        = silv_stand_qmean_diameter(dclass, ntrees_ha),
#'     .by       = c(plot_id, species)
#'   )
#'
#' ## calculate dg for a vector of diameters
#' silv_stand_qmean_diameter(c(12.5, 23.5, 14, 16, 18.5))
silv_stand_qmean_diameter <- function(
  diameter,
  ntrees = NULL
) {

  # 0. Validate inputs
  assert_positive_numeric(diameter, "diameter")

  # 1. Calculate squared mean diameter
  if (is.null(ntrees)) {
    sqrt(
      weighted.mean(
        x = diameter**2,
        w = rep(1, length(diameter))
      )
    )
  } else {
    assert_positive_numeric(ntrees, "ntrees")
    assert_same_length(diameter, ntrees, names = c("diameter", "ntrees"))
    sqrt(
      weighted.mean(
        x = diameter**2,
        w = ntrees
      )
    )
  }

}
