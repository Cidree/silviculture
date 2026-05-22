

Inventory <- S7::new_class(
  name = "Inventory",
  package = "silviculture",
  properties = list(
    dclass_metrics = S7::new_property(S7::class_data.frame, default = quote(data.frame())),
    group_metrics = S7::new_property(S7::class_data.frame, default = quote(data.frame())),
    groups        = S7::new_property(S7::class_character, default = quote(data.frame()))
  ),

  validator = function(self) {
    if (!all(c("dclass", "height", "ntrees", "ntrees_ha", "h0", "dg", "g_ha") %in% names(self@dclass_metrics))) {
      "self@dclass_metrics variables are incorrect"
    } else if (!all(c(
      "d_mean", "d_median", "d_sd", "dg", "h_mean", "h_median",
      "h_sd", "h_lorey", "h0", "ntrees", "ntrees_ha", "g_ha", "spacing") %in% names(self@group_metrics))) {
      "self@group_metrics variables are incorrect"
    }
  }
)




#' Calculates a bunch of forest metrics
#'
#' Summarize forest inventory data calculating most typical variables
#'
#' @param data A tibble of inventory data
#' @template diameter
#' @template height
#' @param plot_size The size of the plot. See [silv_density_ntrees_ha()]
#' @param .groups A character vector with variables to group by (e.g. plot id, tree
#'    species, etc)
#' @template dclass_params
#' @param plot_shape The shape of the sampling plot. Either `circular` or `rectangular`
#' @param which_h0 The method to calculate the dominant height. See [silv_stand_dominant_height()]
#' @param which_spacing A character with the name of the index (either `hart` or `hart-brecking`).
#'    See [silv_density_hart()]
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
silv_summary <- function(
  data,
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
  which_spacing   = "hart"
) {

  # 0. Validate data - rest of inputs are already validated in the functions called inside
  if (!inherits(data, "data.frame")) cli::cli_abort("`data` must be a data frame")


  # 1. Calculate metrics
  dclass_group_metrics <- data |>
    dplyr::mutate(
      dclass = silv_tree_dclass({{ diameter }}, dmin, dmax, class_length, include_lowest)
    ) |>
    dplyr::summarise(
      height = mean({{ height }}, na.rm = TRUE),
      ntrees = dplyr::n(),
      .by    = dplyr::all_of(c(.groups, "dclass"))
    ) |>
    dplyr::mutate(
      ntrees_ha = silv_density_ntrees_ha(ntrees, plot_size, plot_shape),
      h0        = silv_stand_dominant_height(dclass, height, ntrees_ha, which = which_h0),
      dg        = silv_stand_qmean_diameter(dclass, ntrees_ha),
      g_ha      = silv_stand_basal_area(dclass, ntrees_ha),
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
      h_lorey   = silv_stand_lorey_height(height, g_ha, ntrees_ha),
      ntrees    = sum(ntrees),
      ntrees_ha = sum(ntrees_ha),
      g_ha      = sum(g_ha),
      .by       = dplyr::all_of(c(.groups, "h0", "dg"))
    ) |>
    dplyr::mutate(
      spacing   = silv_density_hart(h0, ntrees_ha, which = which_spacing)
    ) |>
    dplyr::select(
      # dplyr::all_of(.groups), dplyr::starts_with("d_"), dg, dplyr::starts_with("h_"), h0, dplyr::everything()
      dplyr::all_of(.groups), dplyr::starts_with("d_"), dg, dplyr::starts_with("h_"), h0, dplyr::everything()
    )

  # 2. Return an Inventory S7 list
  Inventory(
    dclass_metrics = dclass_group_metrics,
    group_metrics  = groups_metrics,
    groups         = .groups
  )
}



