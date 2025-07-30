

#' Classify diameters in classes
#'
#' Classifies the measured diameters into classes of a specified length
#'
#' @param diameter A numeric vector of diameters
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
#'   mutate(dclass = silv_tree_dclass(diameter))
silv_tree_dclass <- function(diameter,
                            dmin             = 7.5,
                            dmax             = NULL,
                            class_length     = 5,
                            include_lowest   = TRUE,
                            return_intervals = FALSE) {

  # 0. Setup and handle errors
  ## 0.1. Handle data type
  if (!is.logical(return_intervals)) cli::cli_abort("The argument `return_intervals` must be TRUE or FALSE")
  if (!is.logical(include_lowest)) cli::cli_abort("The argument `include_lowest` must be TRUE or FALSE")
  if (!is.numeric(diameter)) cli::cli_abort("`diameter` must be a numeric vector")
  if (!is.numeric(dmin)) cli::cli_abort("`dmin` must be a numeric vector")
  if (!is.numeric(dmax) && !is.null(dmax)) cli::cli_abort("`dmax` must be a numeric vector or NULL")
  if (!is.numeric(class_length)) cli::cli_abort("`class_length` must be a numeric vector")
  ## 0.2. Invalid values
  if (any(diameter < 0, na.rm = TRUE)) cli::cli_warn("Any value in `diameter` is less than 0. Review your data.")
  if (dmin <= 0) cli::cli_abort("`dmin` must be greater than 0")
  if (dmax <= 0 && !is.null(dmax)) cli::cli_abort("`dmax` must be greater than 0")
  if (class_length <= 0) cli::cli_abort("`class_length` must be greater than 0")
  ## 0.3. dmax must be > than dmin
  if (dmin >= dmax && is.numeric(dmax)) cli::cli_abort("`dmax` has to be greater than `dmin`")

  # 1. Create intervals depending on user input
  ## - If dmax is NULL, use max diameter from data
  if (is.null(dmax)) {
    cuts_vec <- seq(dmin, max(diameter, na.rm = TRUE) + class_length, class_length)
  } else {
    cli::cli_alert_info(
      "There are {length(diameter[diameter > dmax])} diameter values greater than `dmax = {dmax}`. They will be included in the greatest class."
    )
    diameter[diameter > dmax] <- dmax
    cuts_vec <- c(seq(dmin, dmax, class_length), Inf)
  }

  # 2. Create intervals either ( ] or [ )
  if (include_lowest) {
    intervals_vec <- cut(
      x              = diameter,
      breaks         = cuts_vec,
      right          = FALSE,
      include.lowest = TRUE,
      dig.lab        = 10
    )
  } else {
    intervals_vec <- cut(
      x       = diameter,
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






#' Calculates Basal Area
#'
#' Calculates Basal Area in square meters.
#'
#' @param diameter Numeric vector of diameters or diameter classes
#' @param units The units of the diameter (one of `mm`, `cm`, `dm`, or `m`)
#'
#' @return A numeric vector
#' @export
#'
#' @details
#' The function uses the next formula:
#'
#' \eqn{g = \frac{\pi}{40000} \cdot D^2}
#'
#' where g is the basal area in \eqn{m^2} of one tree, and D is the diameter in `cm`. 
#' 
#' If you want to calculate the basal area for a group of trees (e.g. per hectares),
#' please use [silv_stand_basal_area()]
#' 
#' @seealso [silv_stand_basal_area()]
#'
#' @examples
#' ## calculate individual basal area
#' silv_tree_basal_area(c(23, 11, 43.5, 94))
silv_tree_basal_area <- function(diameter,
                                units = "cm") {

  # 0. Handle errors and set-up
  if (!is.numeric(diameter)) cli::cli_abort("`diameter` must be a numeric vector")
  ## 0.2. Invalid values
  if (any(diameter <= 0, na.rm = TRUE)) cli::cli_warn("Any value in `diameter` is less than 0. Review your data.")

  # 1. Calculate basal area
  switch(units,
    "mm" = (pi / 4) * (diameter / 1000)**2,
    "cm" = (pi / 4) * (diameter / 100)**2,
    "dm" = (pi / 4) * (diameter / 10)**2,
    "m"  = (pi / 4) * diameter**2,
    cli::cli_abort("Invalid `units`. Use <mm>, <cm>, <dm>, or <m>")
  )

}





#' Calculate Tree Volume
#'
#' This function calculates the volume of a tree or logs using different formulas:
#' Pressler, Huber, Smalian, and Newton. The appropriate diameter and height
#' parameters must be provided depending on the selected formula.
#'
#' @param diameter_base A numeric vector. The diameter at the base of the tree
#' (required for Pressler, Smalian, and Newton formulas).
#' @param diameter_top A numeric vector. The diameter at the top of the tree
#' (required for Smalian and Newton formulas).
#' @param diameter_center A numeric vector. The diameter at the center of the
#' tree (required for Huber and Newton formulas).
#' @param diameter A numeric vector. The diameter at breast height (used in
#' Pressler formula if provided instead of `diameter_base`).
#' @param height A numeric vector. The tree or log height (required for all formulas).
#' @param formula Character. The volume formula to use. Options: `"pressler"`,
#' `"huber"`, `"smalian"`, `"newton"`. Default is `"pressler"`.
#' @param ntrees A numeric vector with number of trees of the same dimensions.
#' Default is 1.
#'
#' @return A numeric value representing the tree volume.
#' @examples
#' silv_tree_volume(diameter_base = 30, height = 20, formula = "pressler")
#' silv_tree_volume(diameter_center = 25, height = 15, formula = "huber")
#' silv_tree_volume(diameter_base = 30, diameter_top = 20, height = 20, formula = "smalian")
#'
#' @export
silv_tree_volume <- function(diameter_base   = NULL,
                        diameter_top    = NULL,
                        diameter_center = NULL,
                        diameter        = NULL,
                        height          = NULL,
                        formula         = "pressler",
                        ntrees          = NULL) {

  if (is.null(ntrees)) ntrees <- 1

  if (formula == "pressler") {
    cli::cli_alert_warning("When using Pressler formula, the height is assumed to be Pressler directrix point (i.e., the height at which the diameter of the stem is half the diameter in the base of the tree).")
  }

  ## feedback about units in 0.2.0
  cli::cli_alert_info("Since v. 0.2.0 the diameter is assumed to be in centimeters.")

  ## Apply formula
  volume_vec <- switch(formula,
                       "pressler" = if (!is.null(diameter)) (pi / 4) * (diameter / 100)**2 * (2 / 3) * height * ntrees else (pi / 4) * (diameter_base / 100)**2 * (2 / 3) * height * ntrees,
                       "huber"   = (pi / 4) * (diameter_center / 100)**2 * height * ntrees,
                       "smalian" = (pi / 8) * ((diameter_base / 100)**2 + (diameter_top / 100)**2) * height * ntrees,
                       "newton"  = (pi / 24) * ((diameter_base / 100)**2 + (diameter_top / 100)**2 + 4 * (diameter_center / 100)**2) * height * ntrees
  )

  return(volume_vec)
}

