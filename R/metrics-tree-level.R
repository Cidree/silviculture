

#' Classify diameters in classes
#'
#' Classifies the measured diameters into classes of a specified length
#'
#' @template diameter
#' @template dclass_params
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
silv_tree_dclass <- function(
  diameter,
  dmin             = 7.5,
  dmax             = NULL,
  class_length     = 5,
  include_lowest   = TRUE,  # TODO - this argument is ambiguous
  return_intervals = FALSE
) {

  # 0. Validate inputs
  assert_positive_numeric(diameter, "diameter")
  assert_positive_numeric(dmin, "dmin")
  assert_positive_numeric(class_length, "class_length")
  assert_logical(include_lowest, "include_lowest")
  assert_logical(return_intervals, "return_intervals")
  if (!is.null(dmax)) {
    assert_positive_numeric(dmax, "dmax")
    assert_greater_than(dmax, dmin, "dmax")
  }


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
#' @template diameter
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
silv_tree_basal_area <- function(
  diameter,
  units = "cm"
) {

  # 0. Validate inputs
  assert_positive_numeric(diameter, "diameter")

  # 1. Calculate basal area
  switch(units,
    "mm" = (pi / 4) * (diameter / 1000)**2,
    "cm" = (pi / 4) * (diameter / 100)**2,
    "dm" = (pi / 4) * (diameter / 10)**2,
    "m"  = (pi / 4) * diameter**2,
    cli::cli_abort("Invalid `units`. Use one of {.val {c('mm', 'cm', 'dm', 'm')}}")
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
silv_tree_volume <- function(
    diameter_base   = NULL,
    diameter_top    = NULL,
    diameter_center = NULL,
    diameter        = NULL,
    height          = NULL,
    formula         = "pressler",
    ntrees          = NULL) {

  
  # 0. Validate inputs
  valid_formulas <- c("pressler", "huber", "smalian", "newton")
  if (!formula %in% valid_formulas) {
    cli::cli_abort("Invalid `formula`. Use one of {.val {valid_formulas}}")
  }

  ## Resolve defaults
  if (is.null(ntrees)) ntrees <- 1

  ## For pressler, diameter_base serves as fallback for diameter
  if (formula == "pressler" && is.null(diameter)) diameter <- diameter_base

  ## Declare which parameters each formula requires
  required <- list(
    pressler = c("height", "ntrees", "diameter"),
    huber    = c("height", "ntrees", "diameter_center"),
    smalian  = c("height", "ntrees", "diameter_base", "diameter_top"),
    newton   = c("height", "ntrees", "diameter_base", "diameter_top", "diameter_center")
  )

  ## Assert that required parameters are provided and valid
  params <- list(
    height          = height,
    ntrees          = ntrees,
    diameter        = diameter,
    diameter_base   = diameter_base,
    diameter_top    = diameter_top,
    diameter_center = diameter_center
  )

  for (param in required[[formula]]) {
    assert_positive_numeric(params[[param]], param)
  }

  if (formula == "pressler") {
    cli::cli_alert_warning(
      "When using Pressler formula, height is assumed to be the Pressler \\
       directrix point (i.e., the height at which the stem diameter is half \\
       the base diameter)."
    )
  }

  # Convert diameter in cm to area in m^2: (d/100)^2 * pi/4
  area <- function(d) (pi / 4) * (d / 100)^2

  volume_vec <- switch(
    formula,
    pressler = (2/3) * area(diameter) * height * ntrees,
    huber    = area(diameter_center) * height * ntrees,
    smalian  = (1/2) * (area(diameter_base) + area(diameter_top)) * height * ntrees,
    newton   = (1/6) * (area(diameter_base) + area(diameter_top) + 4 * area(diameter_center)) * height * ntrees
  )

  volume_vec
}

