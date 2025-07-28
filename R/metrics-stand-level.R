
#' Calculates Basal Area
#'
#' Calculates Basal Area in square meters.
#'
#' @param diameter Numeric vector of diameters or diameter classes
#' @param ntrees Numeric vector with number of trees of the diameter class per
#'    hectare. If `ntrees = NULL`, the function will assume that each diameter
#'    corresponds to only one tree
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
#' recommended to use the squared mean diameter calculated with [silv_stand_qmean_diameter].
#' 
#' Note that if \code{ntrees = NULL}, the output of the function will be exactly
#' the same as in [silv_tree_basal_area].
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
silv_stand_basal_area <- function(diameter,
                                  ntrees = NULL,
                                  units = "cm") {

  # 0. Handle errors and set-up
  if (is.numeric(ntrees) && length(ntrees) != length(diameter)) cli::cli_abort("`ntrees` must have the same length as `diameter` or be NULL")
  if (!is.numeric(diameter)) cli::cli_abort("`diameter` must be a numeric vector")
  ## 0.2. Invalid values
  if (any(diameter <= 0, na.rm = TRUE)) cli::cli_warn("Any value in `diameter` is less than 0. Review your data.")
  ## 0.3. If ntrees = NULL, only one tree assumed
  if (is.null(ntrees)) ntrees <- rep(1, length(diameter))

  # 1. Calculate basal area
  switch(units,
    "mm" = (pi / 4) * (diameter / 1000)**2,
    "cm" = (pi / 4) * (diameter / 100)**2,
    "dm" = (pi / 4) * (diameter / 10)**2,
    "m"  = (pi / 4) * diameter**2,
    cli::cli_abort("Invalid `units`. Use <mm>, <cm>, <dm>, or <m>")
  )

}





#' Calculates the dominant height
#'
#' Calculates the dominant height using the Assman equation or the Hart equation
#'
#' @param diameter Numeric vector with diameter classes
#' @param height Numeric vector with averaged heights by diameter class
#' @param ntrees Optional. Numeric vector with number of trees per hectare.
#' Use this argument when you have aggregated data by diametric classes (see details).
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
silv_stand_dominant_height <- function(diameter,
                                 height,
                                 ntrees = NULL,
                                 which = "assman") {

  # 0. Handle errors and setup
  ## 0.1. Errors
  if (!tolower(which) %in% c("assman", "hart")) cli::cli_abort("`which` must be either <assman> or <hart>.")
  if (!is.numeric(diameter)) cli::cli_abort("`diameter` must be a numeric vector")
  if (!is.numeric(height)) cli::cli_abort("`height` must be a numeric vector")
  ## 0.2. Invalid values
  if (any(diameter <= 0, na.rm = TRUE)) cli::cli_warn("Any value in `diameter` is less than 0. Review your data.")
  if (any(height <= 0, na.rm = TRUE)) cli::cli_warn("Any value in `height` is less than 0. Review your data.")

  # 1. Create a data frame with input variables
  if (is.null(ntrees)) {
    data <- data.frame(
      d  = diameter,
      h  = height,
      nt = 1
    )
  } else {
    data <- data.frame(
      d  = diameter,
      h  = height,
      nt = ntrees
    )
  }


  # 2. Calculate dominant height
  if (tolower(which) == "assman") {
    d0 <- data |>
      ## sort descending by diameter class
      dplyr::arrange(dplyr::desc(d)) |>
      dplyr::mutate(
        .cumtrees = cumsum(nt),
        .nmax     = which(.cumtrees >= 100)[1],
        .nmax     = if (is.na(.nmax[1])) which.max(.cumtrees) else .nmax,
        .do       = calc_dominant_height(.nmax, nt, h)
      ) |>
      dplyr::pull(.do)
  } else {
    d0 <- data |>
      ## sort descending by height
      dplyr::arrange(dplyr::desc(h)) |>
      dplyr::mutate(
        .cumtrees = cumsum(nt),
        .nmax     = which(.cumtrees >= 100)[1],
        .nmax     = if (is.na(.nmax[1])) which.max(.cumtrees) else .nmax,
        .do       = calc_dominant_height(.nmax, nt, h)
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
#' @param height Numeric vector of heights
#' @param g Numeric vector of basal areas
#' @param ntrees Optional. Numeric vector of number of trees per hectare.
#' Use this argument when you have aggregated data by diametric classes (see details).
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
silv_stand_lorey_height <- function(height, g, ntrees = NULL) {

  # 0. Handle errors
  if (length(height) != length(g)) cli::cli_abort("`height` and `g` must have the same length")
  if (is.numeric(ntrees) && length(ntrees) != length(g)) cli::cli_abort("`ntrees` must have the same length as `height` and `g`, or be NULL")

  # 1. Calculate
  if (is.null(ntrees)) {
    weighted.mean(height, g)
  } else {
    weighted.mean(height, g * ntrees)
  }

}





#' Calculates the quadratic mean diameter (QMD)
#'
#' @param diameter Numeric vector of diameters or diameter classes
#' @param ntrees Numeric vector with number of trees of the diameter class per
#' hectare. If `ntrees = NULL`, the function will assume that each diameter
#' corresponds to only one tree.
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
silv_stand_qmean_diameter <- function(diameter,
                                  ntrees = NULL) {

  # 0. Handle errors and setup
  if (is.numeric(ntrees) && length(ntrees) != length(diameter)) cli::cli_abort("`ntrees` must have the same length as `diameter` or be NULL")
  if (!is.numeric(diameter)) cli::cli_abort("`diameter` must be a numeric vector")
  ## 0.2. Invalid values
  if (any(diameter <= 0, na.rm = TRUE)) cli::cli_warn("Any value in `diameter` is less than 0. Review your data.")

  # 1. Calculate squared mean diameter
  if (is.null(ntrees)) {
    sqrt(
      weighted.mean(
        x = diameter**2,
        w = rep(1, length(diameter))
      )
    )
  } else {
    sqrt(
      weighted.mean(
        x = diameter**2,
        w = ntrees
      )
    )
  }

}