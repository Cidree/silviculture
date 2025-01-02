
#' Classify diameters in classes
#'
#' Classifies the measured diameters into classes of a specified length
#'
#' @param x A numeric vector of diameters
#' @param dmin The minimum inventory diameter in centimeters
#' @param dmax The maximum inventory diameter in centimeters. Values that
#'    are greater than `dmax` are included in the greatest class
#' @param class_length The length of the class in centimeters
#' @param include_lowest Logical. If TRUE (the default), the intervals are
#'    `[dim1, dim2)`. If FALSE, the intervals are `(dim1, dim2]`
#' @param return_intervals If FALSE, it returns the intervals. Otherwise (the
#'    default), it returns the class center
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' library(dplyr)
#' inventory_samples |>
#'   mutate(dclass = silv_diametric_class(diameter))
silv_diametric_class <- function(x,
                                 dmin             = 7.5,
                                 dmax             = NULL,
                                 class_length     = 5,
                                 include_lowest   = TRUE,
                                 return_intervals = FALSE) {

  # 0. Setup and handle errors
  ## 0.1. Handle data type
  if (!is.logical(return_intervals)) cli::cli_abort("The argument `return_intervals` must be TRUE or FALSE")
  if (!is.logical(include_lowest)) cli::cli_abort("The argument `include_lowest` must be TRUE or FALSE")
  if (!is.numeric(x)) cli::cli_abort("`x` must be a numeric vector")
  if (!is.numeric(dmin)) cli::cli_abort("`dmin` must be a numeric vector")
  if (!is.numeric(dmax) && !is.null(dmax)) cli::cli_abort("`dmax` must be a numeric vector or NULL")
  if (!is.numeric(class_length)) cli::cli_abort("`class_length` must be a numeric vector")
  ## 0.2. Invalid values
  if (any(x < 0)) cli::cli_warn("Any value in `x` is less than 0. Review your data.")
  if (dmin <= 0) cli::cli_abort("`dmin` must be greater than 0")
  if (dmax <= 0 && !is.null(dmax)) cli::cli_abort("`dmax` must be greater than 0")
  if (class_length <= 0) cli::cli_abort("`class_length` must be greater than 0")
  ## 0.3. dmax must be > than dmin
  if (dmin >= dmax && is.numeric(dmax)) cli::cli_abort("`dmax` has to be greater than `dmin`")

  # 1. Create intervals depending on user input
  ## - If dmax is NULL, use max diameter from data
  if (is.null(dmax)) {
    cuts_vec <- seq(dmin, max(x, na.rm = TRUE) + class_length, class_length)
  } else {
    cli::cli_alert_info(
      "There are {length(x[x > dmax])} diameter values greater than `dmax = {dmax}`. They will be included in the greatest class."
    )
    x[x > dmax] <- dmax
    cuts_vec <- c(seq(dmin, dmax, class_length), Inf)
  }

  # 2. Create intervals either ( ] or [ )
  if (include_lowest) {
    intervals_vec <- cut(
      x              = x,
      breaks         = cuts_vec,
      right          = FALSE,
      include.lowest = TRUE,
      dig.lab        = 10
    )
  } else {
    intervals_vec <- cut(
      x       = x,
      breaks  = cuts_vec,
      dig.lab = 10
    )
  }

  # 3. Return intervals or class center?
  if (!return_intervals) {
    intervals_vec <- cuts_vec[as.numeric(intervals_vec)] + (class_length / 2)
  } else {
    ## Drop redundant levels
    intervals_vec <- droplevels(intervals_vec)
  }

  # 4. Return object
  return(intervals_vec)

}





#' Calculates number of trees per hectare
#'
#' Calculates number of trees per hectare for a given plot size and shape
#'
#' @param x A numeric vector representing the number of trees in a sampling plot
#' @param plot_size A numeric vector of length one for circular radius in meters;
#'    or a numeric vector of length two for each side of a rectangular plot shape
#' @param plot_shape The shape of the sampling plot. Either `circular` or `rectangular`
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' library(dplyr)
#' ## Circular plot of 10 meters radius
#' inventory_samples |>
#'   count(plot_id, species) |>
#'   mutate(
#'     ntrees_ha = silv_ntrees_ha(n, plot_size = 10)
#'   )
#'
#' ## Rectangular plot of 10x15 meters
#' inventory_samples |>
#'   count(plot_id, species) |>
#'   mutate(
#'     ntrees_ha = silv_ntrees_ha(
#'       n,
#'       plot_size = c(10, 15),
#'       plot_shape = "rectangular"
#'      )
#'   )
silv_ntrees_ha <- function(x,
                           plot_size,
                           plot_shape = "circular") {

  # 0. Handle errors
  stopifnot(plot_shape %in% c("circular", "rectangular"))
  if (length(plot_size) == 1 && plot_size <= 0) cli::cli_abort("`plot_size` has to be greater than 0")

  # 1. Calculate ntrees in ha
  if (plot_shape == "circular") {
    x * 10000 / (pi * plot_size**2)
  } else {
    x * 10000 / prod(plot_size)
  }

  # 2. Convert to ntrees/acres
  # if (unit == "acres") {
  #   ntrees <- ntrees / 2.4710538147
  # }

  # 3. Return object
  # return(ntrees)


}





#' Calculates the dominant height
#'
#' Calculates the dominant height using the Assman equation of the Hart equation
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
#' @references Assmann, E. (1970) The principles of forest yield study: Studies in the
#' organic production, structure, increment, and yield of forest stands. Pergamon Press, Oxford.
#'
#' @return A numeric vector
#' @export
#'
#' @details
#' When \code{ntrees = NULL}, the function will assume that each diameter and height
#' belongs to only one trees. If you have data aggregated by hectare, you'll use the
#' number of trees per hectare in this argument.
#'
#'
#' @examples
#' ## calculate h0 for inventory data grouped by plot_id and species
#' library(dplyr)
#' inventory_samples |>
#' mutate(dclass = silv_diametric_class(diameter)) |>
#'   summarise(
#'     height = mean(height, na.rm = TRUE),
#'     ntrees = n(),
#'     .by    = c(plot_id, species, dclass)
#'   ) |>
#'   mutate(
#'     ntrees_ha = silv_ntrees_ha(ntrees, plot_size = 10),
#'     h0        = silv_dominant_height(dclass, height, ntrees_ha),
#'     .by       = c(plot_id, species)
#'   )
silv_dominant_height <- function(diameter,
                                 height,
                                 ntrees = NULL,
                                 which = "assman") {

  # 0. Handle errors and setup
  ## 0.1. Errors
  if (!tolower(which) %in% c("assman", "hart")) cli::cli_abort("`which` must be either <assman> or <hart>.")
  if (!is.numeric(diameter)) cli::cli_abort("`diameter` must be a numeric vector")
  if (!is.numeric(height)) cli::cli_abort("`height` must be a numeric vector")
  ## 0.2. Invalid values
  if (any(diameter <= 0)) cli::cli_warn("Any value in `diameter` is less than 0. Review your data.")
  if (any(height <= 0)) cli::cli_warn("Any value in `height` is less than 0. Review your data.")

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
#'   mutate(g = silv_basal_area(diameter)) |>
#'   summarise(
#'     lh  = silv_lorey_height(height, g),
#'     .by = c(plot_id, species)
#'   )
silv_lorey_height <- function(height, g, ntrees = NULL) {

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




#' Calculates the squared mean diameter
#'
#' @param diameter Numeric vector of diameters or diameter classes
#' @param ntrees Numeric vector with number of trees of the diameter class per
#'    hectare. If `ntrees = NULL`, the function will assume that each diameter
#'    corresponds to only one tree. Therefore, basal area will be calculated
#'    for each individual tree
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' ## calculate dg for inventory data grouped by plot_id and species
#' library(dplyr)
#' inventory_samples |>
#' mutate(dclass = silv_diametric_class(diameter)) |>
#'   summarise(
#'     height = mean(height, na.rm = TRUE),
#'     ntrees = n(),
#'     .by    = c(plot_id, species, dclass)
#'   ) |>
#'   mutate(
#'     ntrees_ha = silv_ntrees_ha(ntrees, plot_size = 10),
#'     h0        = silv_dominant_height(dclass, height, ntrees_ha),
#'     dg        = silv_sqrmean_diameter(dclass, ntrees_ha),
#'     .by       = c(plot_id, species)
#'   )
#'
#' ## calculate dg for a vector of diameters
#' silv_sqrmean_diameter(c(12.5, 23.5, 14, 16, 18.5))
silv_sqrmean_diameter <- function(diameter,
                                  ntrees = NULL) {

  # 0. Handle errors and setup
  if (is.numeric(ntrees) && length(ntrees) != length(diameter)) cli::cli_abort("`ntrees` must have the same length as `diameter` or be NULL")
  if (!is.numeric(diameter)) cli::cli_abort("`diameter` must be a numeric vector")
  ## 0.2. Invalid values
  if (any(diameter <= 0)) cli::cli_warn("Any value in `diameter` is less than 0. Review your data.")

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





#' Calculates Basal Area
#'
#' Calculates Basal Area in square meters.
#'
#' @param diameter Numeric vector of diameters or diameter classes
#' @param ntrees Numeric vector with number of trees of the diameter class per
#'    hectare. If `ntrees = NULL`, the function will assume that each diameter
#'    corresponds to only one tree. Therefore, basal area will be calculated
#'    for each individual tree
#' @param units The units of the diameter (one of `cm`, `mm`, or `m`)
#'
#' @return A numeric vector
#' @export
#'
#' @details
#' The function uses the next formula:
#'
#' \eqn{G = \frac{\pi}{40000} \cdot D^2}
#'
#' where G is the basal area in \eqn{m^2}, and D is the diameter in the `units`
#' specified in the function. It is recommended to use the squared mean diameter
#' calculated with [silv_sqrmean_diameter]
#'
#' @examples
#' ## calculate G for inventory data grouped by plot_id and species
#' library(dplyr)
#' inventory_samples |>
#' mutate(dclass = silv_diametric_class(diameter)) |>
#'   summarise(
#'     height = mean(height, na.rm = TRUE),
#'     ntrees = n(),
#'     .by    = c(plot_id, species, dclass)
#'   ) |>
#'   mutate(
#'     ntrees_ha = silv_ntrees_ha(ntrees, plot_size = 10),
#'     dg        = silv_sqrmean_diameter(dclass, ntrees_ha),
#'     g         = silv_basal_area(dclass, ntrees_ha),
#'     .by       = c(plot_id, species)
#'   )
#'
#' ## calculate individual basal area
#' silv_basal_area(c(23, 11, 43.5, 94))
silv_basal_area <- function(diameter,
                            ntrees = NULL,
                            units = "cm") {

  # 0. Handle errors and set-up
  if (is.numeric(ntrees) && length(ntrees) != length(diameter)) cli::cli_abort("`ntrees` must have the same length as `diameter` or be NULL")
  if (!is.numeric(diameter)) cli::cli_abort("`diameter` must be a numeric vector")
  ## 0.2. Invalid values
  if (any(diameter <= 0)) cli::cli_warn("Any value in `diameter` is less than 0. Review your data.")
  ## 0.3. If ntrees = NULL, only one tree assumed
  if (is.null(ntrees)) ntrees <- rep(1, length(diameter))

  # 1. Calculate basal area
  switch(units,
    "cm" = (pi / 4) * (diameter / 100)**2 * ntrees,
    "mm" = (pi / 4) * (diameter / 1000)**2 * ntrees,
    "m"  = (pi / 4) * diameter**2 * ntrees,
    cli::cli_abort("Invalid `units`. Use <cm>, <mm>, or <m>")
  )

}





#' Hart or Hart-Becking spacing index
#'
#' Calculates the Hart Index or the Hart-Becking Index for even-aged stands
#'
#' @param h0 Numeric vector with dominant height
#' @param ntrees Numeric vector with number of trees of the dominant height per
#'    hectare
#' @param which A character with the name of the index (either `hart` or `hart-brecking`).
#'    See details
#'
#' @return A numeric vector
#' @export
#'
#' @details
#' The spacing index can be used to determine whether a thinning is needed or not,
#' and also to determine how intense it should be.
#'
#' - \bold{Hart Index}: it assumes even-aged stands with square planting pattern.
#'
#' - \bold{Hart-Brecking Index}: it assumes triangular planting pattern.
#'
#'
#' @references Assmann, E. (1970) The principles of forest yield study: Studies in the
#' organic production, structure, increment, and yield of forest stands. Pergamon Press, Oxford.
#'
#' @examples
#' library(dplyr)
#' ## Calculate spacing index for each plot
#' inventory_samples |>
#'   summarise(
#'     h0     = silv_dominant_height(diameter, height),
#'     ntrees = n(),
#'     .by    = plot_id
#'   ) |>
#'   ## calculate number of trees per hectare
#'   mutate(ntrees_ha = silv_ntrees_ha(ntrees, plot_size = 14.1)) |>
#'   mutate(spacing = silv_spacing_index(h0, ntrees_ha))
silv_spacing_index <- function(h0,
                               ntrees,
                               which = "hart") {

  # 0. Errors
  if (!is.numeric(ntrees)) cli::cli_abort("ntrees` must be numeric")
  if (!is.numeric(h0)) cli::cli_abort("`h0` must be numeric")
  if (length(h0) != length(ntrees)) cli::cli_abort("`h0` and `ntrees` must have the same length")

  # 1. Calculate spacing index
  switch(which,
    "hart"         = 10000 / h0 / sqrt(ntrees),
    "hart-becking" = sqrt(20000 / (ntrees * sqrt(3))) / h0 * 100,
    cli::cli_abort("`which` must be either <hart> or <hart-becking>")
  )

}


#' Calculates a bunch of forest metrics
#'
#' Summarize forest inventory data calculating most typical variables
#'
#' @param data A tibble
#' @param diameter A column with inventory diameters
#' @param height A column with inventory heights
#' @param plot_size The size of the plot. See \link{silv_ntrees_ha}
#' @param .groups A character vector with variables to group by (e.g. plot id, tree
#'    species, etc)
#' @param dmin The minimum inventory diameter in centimeters
#' @param dmax The maximum inventory diameter in centimeters. Values that
#'    are greater than `dmax` are included in the greatest class
#' @param class_length The length of the class in centimeters
#' @param include_lowest Logical. If TRUE (the default), the intervals are
#'    `[dim1, dim2)`. If FALSE, the intervals are `(dim1, dim2]`
#' @param plot_shape The shape of the sampling plot. Either `circular` or `rectangular`
#' @param which_h0 The method to calculate the dominant height. See \link{silv_dominant_height}
#' @param which_spacing A character with the name of the index (either `hart` or `hart-brecking`).
#'    See \link{silv_spacing_index}
#'
#' @return A list with two tibbles
#' @export
#'
#' @details
#' The function calculates many inventory parameters and returns two tibbles:
#'
#' - \bold{dclass_metrics}: metrics summarized by .groups and diametric classes
#'
#' - \bold{group_metrics}: metrics summarized by .groups
#'
#'
#' @examples
#' silv_summary(
#'   data      = inventory_samples,
#'   diameter  = diameter,
#'   height    = height,
#'   plot_size = 10,
#'   .groups   = c("plot_id", "species")
#'  )
silv_summary <- function(data,
                         diameter,
                         height,
                         plot_size,
                         .groups         = NULL,
                         plot_shape      = "circular",
                         dmin            = 7.5,
                         dmax            = NULL,
                         class_length    = 5,
                         include_lowest  = TRUE,
                         which_h0        = "assman",
                         which_spacing   = "hart") {

  # 0. Handle errors and setup
  ## 0.1. Errors
  if (!which_h0 %in% c("assman", "hart")) cli::cli_abort("The argument `which_h0` must be either <assman> or <hart>.")
  if (!which_spacing %in% c("hart", "hart-brecking")) cli::cli_abort("The argument `which_spacing` must be either <hart-brecking> or <hart>.")

  # calculate metrics
  dclass_group_metrics <- data |>
    dplyr::mutate(
      dclass = silv_diametric_class({{ diameter }}, dmin, dmax, class_length, include_lowest)
    ) |>
    dplyr::summarise(
      height = mean({{ height }}, na.rm = TRUE),
      ntrees = dplyr::n(),
      .by    = dplyr::all_of(c(.groups, "dclass"))
    ) |>
    dplyr::mutate(
      ntrees_ha = silv_ntrees_ha(ntrees, plot_size, plot_shape),
      h0        = silv_dominant_height(dclass, height, ntrees_ha, which = which_h0),
      dg        = silv_sqrmean_diameter(dclass, ntrees_ha),
      g_ha      = silv_basal_area(dclass, ntrees_ha),
      .by       = .groups
    ) |>
    dplyr::arrange(dplyr::across(.groups))

  # groups metrics
  groups_metrics <- dclass_group_metrics |>
    dplyr::summarise(
      d_mean    = weighted.mean(dclass, ntrees_ha),
      d_median  = weighted_median(dclass, ntrees_ha),
      d_sd      = weighted_sd(dclass, ntrees_ha),
      h_mean    = weighted.mean(height, ntrees_ha),
      h_median  = weighted_median(height, ntrees_ha),
      h_sd      = weighted_sd(height, ntrees_ha),
      h_lorey   = silv_lorey_height(height, g_ha, ntrees_ha),
      ntrees    = sum(ntrees),
      ntrees_ha = sum(ntrees_ha),
      g_ha      = sum(g_ha),
      .by       = dplyr::all_of(c(.groups, "h0", "dg"))
    ) |>
    dplyr::mutate(
      spacing   = silv_spacing_index(h0, ntrees_ha, which = which_spacing)
    ) |>
    dplyr::select(
      .groups, dplyr::starts_with("d_"), dg, dplyr::starts_with("h_"), h0, dplyr::everything()
    )

  # return list
  list(
    dclass_metrics = dclass_group_metrics,
    group_metrics  = groups_metrics
  )
}


