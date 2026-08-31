

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
      "h_sd", "h_lorey", "h0", "ntrees", "ntrees_ha", "g_ha", "spacing", "slenderness") %in% names(self@group_metrics))) {
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
      spacing     = silv_density_hart(h0, ntrees_ha, which = which_spacing),
      slenderness = silv_stand_slenderness(h_mean, dg)
    ) |>
    dplyr::select(
      dplyr::all_of(.groups), dplyr::starts_with("d_"), dg, dplyr::starts_with("h_"), h0, dplyr::everything()
    )

  # 2. Return an Inventory S7 list
  Inventory(
    dclass_metrics = dclass_group_metrics,
    group_metrics  = groups_metrics,
    groups         = .groups
  )
}


#' Summarize plot data by species
#'
#' Calculates the number of trees and basal area per hectare for each species
#' in a plot, and optionally provides the top species by basal area.
#'
#' @param data A data frame or tibble of tree-level data
#' @param plot_id Unquoted column name with the plot identifier
#' @param species Unquoted column name with the species identifier
#' @param expan Unquoted column name with the expansion factor (trees/ha). If `NULL`, it is assumed each row represents 1 tree/ha.
#' @param g Unquoted column name with the basal area per tree (m²/ha). If `NULL`, `diameter` must be provided.
#' @param diameter Unquoted column name with the diameter (cm). Used to calculate basal area if `g` is `NULL`.
#' @param top_n Number of top species to pivot into columns (default: 3). If `0`, no pivoting is performed.
#'
#' @return A tibble with species-level summaries.
#' @export
#'
#' @examples
#' library(dplyr)
#' inventory_samples |>
#'   mutate(expan = silv_density_ntrees_ha(1, 10)) |>
#'   silv_summary_species(plot_id, species, expan, diameter = diameter, top_n = 3)
silv_summary_species <- function(data, plot_id, species, expan = NULL, g = NULL, diameter = NULL, top_n = 3) {
  
  if (!inherits(data, "data.frame")) cli::cli_abort("`data` must be a data frame")
  
  plot_id_sym <- rlang::enquo(plot_id)
  species_sym <- rlang::enquo(species)
  expan_sym <- rlang::enquo(expan)
  g_sym <- rlang::enquo(g)
  diameter_sym <- rlang::enquo(diameter)
  
  if (rlang::quo_is_null(expan_sym)) {
    data <- dplyr::mutate(data, .expan = 1)
  } else {
    data <- dplyr::mutate(data, .expan = !!expan_sym)
  }
  
  if (rlang::quo_is_null(g_sym)) {
    if (rlang::quo_is_null(diameter_sym)) {
      cli::cli_abort("Either {.arg g} or {.arg diameter} must be provided.")
    }
    data <- dplyr::mutate(data, .g = silv_stand_basal_area(diameter = !!diameter_sym, ntrees = .expan))
  } else {
    data <- dplyr::mutate(data, .g = !!g_sym)
  }
  
  res <- data |>
    dplyr::group_by(!!plot_id_sym, !!species_sym) |>
    dplyr::summarise(
      N_sp = sum(.expan, na.rm = TRUE),
      G_sp = sum(.g, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(!!plot_id_sym, dplyr::desc(G_sp))
    
  if (top_n > 0) {
    if (!requireNamespace("tidyr", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg tidyr} is required for pivoting. Please install it or set {.arg top_n} = 0.")
    }
    
    res_pivot <- res |>
      dplyr::group_by(!!plot_id_sym) |>
      dplyr::mutate(.rank = dplyr::row_number()) |>
      dplyr::filter(.rank <= top_n) |>
      dplyr::ungroup()
      
    res_sp <- res_pivot |>
      tidyr::pivot_wider(
        id_cols = !!plot_id_sym,
        names_from = .rank,
        values_from = c(!!species_sym, G_sp, N_sp)
      )
      
    sp_col_name <- rlang::as_name(species_sym)
    # pivot_wider generates 'species_1', 'G_sp_1', 'N_sp_1', etc.
    names(res_sp) <- gsub(paste0("^", sp_col_name, "_"), "sp", names(res_sp))
    names(res_sp) <- gsub("_sp_", "_sp", names(res_sp))
    return(res_sp)
  }
  
  return(res)
}


#' Summarize plot data by mortality status
#'
#' Calculates the number of trees and basal area per hectare for live and dead trees in a plot.
#'
#' @param data A data frame or tibble of tree-level data
#' @param plot_id Unquoted column name with the plot identifier
#' @param dead Unquoted column name indicating if the tree is dead (e.g., TRUE/FALSE or 1/0)
#' @param expan Unquoted column name with the expansion factor (trees/ha). If `NULL`, it is assumed each row represents 1 tree/ha.
#' @param g Unquoted column name with the basal area per tree (m²/ha). If `NULL`, `diameter` must be provided.
#' @param diameter Unquoted column name with the diameter (cm). Used to calculate basal area if `g` is `NULL`.
#'
#' @return A tibble with plot-level mortality summaries.
#' @export
#'
#' @examples
#' library(dplyr)
#' inventory_samples |>
#'   mutate(expan = silv_density_ntrees_ha(1, 10)) |>
#'   mutate(is_dead = sample(c(TRUE, FALSE), n(), replace = TRUE, prob = c(0.1, 0.9))) |>
#'   silv_summary_mortality(plot_id, is_dead, expan, diameter = diameter)
silv_summary_mortality <- function(data, plot_id, dead, expan = NULL, g = NULL, diameter = NULL) {
  
  if (!inherits(data, "data.frame")) cli::cli_abort("`data` must be a data frame")
  
  plot_id_sym <- rlang::enquo(plot_id)
  dead_sym <- rlang::enquo(dead)
  expan_sym <- rlang::enquo(expan)
  g_sym <- rlang::enquo(g)
  diameter_sym <- rlang::enquo(diameter)
  
  if (rlang::quo_is_null(expan_sym)) {
    data <- dplyr::mutate(data, .expan = 1)
  } else {
    data <- dplyr::mutate(data, .expan = !!expan_sym)
  }
  
  if (rlang::quo_is_null(g_sym)) {
    if (rlang::quo_is_null(diameter_sym)) {
      cli::cli_abort("Either {.arg g} or {.arg diameter} must be provided.")
    }
    data <- dplyr::mutate(data, .g = silv_stand_basal_area(diameter = !!diameter_sym, ntrees = .expan))
  } else {
    data <- dplyr::mutate(data, .g = !!g_sym)
  }
  
  res <- data |>
    dplyr::mutate(.status = ifelse(!!dead_sym, "dead", "alive")) |>
    dplyr::group_by(!!plot_id_sym) |>
    dplyr::summarise(
      N_alive = sum(.expan[.status == "alive"], na.rm = TRUE),
      N_dead  = sum(.expan[.status == "dead"], na.rm = TRUE),
      G_alive = sum(.g[.status == "alive"], na.rm = TRUE),
      G_dead  = sum(.g[.status == "dead"], na.rm = TRUE),
      .groups = "drop"
    )
    
  return(res)
}


