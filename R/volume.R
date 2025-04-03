
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
#' silv_volume(diameter_base = 30, height = 20, formula = "pressler")
#' silv_volume(diameter_center = 25, height = 15, formula = "huber")
#' silv_volume(diameter_base = 30, diameter_top = 20, height = 20, formula = "smalian")
#'
#' @export
silv_volume <- function(diameter_base   = NULL,
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

  ## Apply formula
  volume_vec <- switch(formula,
                       "pressler" = if (!is.null(diameter)) (pi / 4) * diameter**2 * (2 / 3) * height else (pi / 4) * diameter_base**2 * (2 / 3) * height,
                       "huber"   = (pi / 4) * diameter_center**2 * height * ntrees,
                       "smalian" = (pi / 8) * (diameter_base**2 + diameter_top**2) * height * ntrees,
                       "newton"  = (pi / 24) * (diameter_base**2 + diameter_top**2 + 4 * diameter_center**2) * height * ntrees
  )

  return(volume_vec)
}
