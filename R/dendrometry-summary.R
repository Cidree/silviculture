
#' @title Inventory class
#'
#' @description
#' A list containing forest inventory data summaries. It includes data by
#' diametric class (`dclass_metrics`) and by plot and species (`group_metrics`).
#'
#' @param dclass_metrics A data.frame summarised by diametric class with variables
#' such as plot_id, species, dclass, height, ntrees, ntrees_ha, h0, dg, and g_ha.
#' @param group_metrics A data.frame summarised by plot and species with variables
#' such as plot_id, species, d_mean, d_median, d_sd, dg, h_mean, h_median, h_sd,
#' h_lorey, h0, ntrees, ntrees_ha, g_ha, and spacing.
#'
#' @return An S7 `Inventory` object, which contains the inventory data.
#'
Inventory <- S7::new_class(
  name = "Inventory",
  package = "silviculture",
  properties = list(
    dclass_metrics = S7::new_property(S7::class_data.frame, default = quote(data.frame())),
    group_metrics = S7::new_property(S7::class_data.frame, default = quote(data.frame()))
  ),

  validator = function(self) {
    if (!all(names(self@dclass_metrics) %in% c("plot_id", "species", "dclass", "height", "ntrees", "ntrees_ha", "h0", "dg", "g_ha"))) {
      "self@dclass_metrics variables are incorrect"
    } else if (!all(names(self@group_metrics) %in% c(
      "plot_id", "species", "d_mean", "d_median", "d_sd", "dg", "h_mean", "h_median",
      "h_sd", "h_lorey", "h0", "ntrees", "ntrees_ha", "g_ha", "spacing"))) {
      "self@group_metrics variables are incorrect"
    }
  }
)




#' Calculates a bunch of forest metrics
#'
#' Summarize forest inventory data calculating most typical variables
#'
#' @param data A tibble of inventory data
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
#' @include utils-not-exported.R
#' @return an S7 `Inventory` list with 2 `tibbles`
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
      # .by       = c(dplyr::all_of(.groups), "h0", "dg")
    ) |>
    dplyr::mutate(
      spacing   = silv_spacing_index(h0, ntrees_ha, which = which_spacing)
    ) |>
    dplyr::select(
      # dplyr::all_of(.groups), dplyr::starts_with("d_"), dg, dplyr::starts_with("h_"), h0, dplyr::everything()
      .groups, dplyr::starts_with("d_"), dg, dplyr::starts_with("h_"), h0, dplyr::everything()
    )

  # return list
  Inventory(
    dclass_metrics = dclass_group_metrics,
    group_metrics = groups_metrics
  )
}


