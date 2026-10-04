

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
#' @param volume Unquoted column name with individual tree volume, optional.
#' @param volume_units Character. Units of the individual tree volume (`"dm3"` or `"m3"`). Default is `"dm3"`.
#' @param biomass Unquoted column name with individual tree biomass (kg), optional.
#' @param carbon Unquoted column name with individual tree carbon (kg), optional.
#' @param predict_volume Logical. If TRUE, predicts tree volume using [silv_predict_snfi_volume()] (default: FALSE).
#' @param province Unquoted column name or scalar string/integer with the province code/name.
#' @param species Unquoted column name with tree species identifier.
#' @param predict_biomass Logical. If TRUE, predicts tree biomass using [silv_predict_biomass_auto()] (default: FALSE).
#' @param biomass_component Character. Tree component to predict for biomass (default: `"tree"`).
#' @param predict_carbon Logical. If TRUE, predicts tree carbon using [silv_predict_carbon_auto()] (default: FALSE).
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
#' Volume is reported at stand level in \eqn{m^3/\text{ha}} (`v_ha`).
#' Biomass and carbon are reported at stand level in \eqn{t/\text{ha}} (`w_ha`, `c_ha`).
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
  plot_size         = NULL,
  .groups           = NULL,
  plot_shape        = c("circular", "rectangular", "snfi"),
  dmin              = 7.5,
  dmax              = NULL,
  class_length      = 5,
  include_lowest    = TRUE,
  which_h0          = "assman",
  which_spacing     = "hart",
  volume            = NULL,
  volume_units      = "dm3",
  biomass           = NULL,
  carbon            = NULL,
  predict_volume    = FALSE,
  province          = NULL,
  species           = NULL,
  predict_biomass   = FALSE,
  biomass_component = "tree",
  predict_carbon    = FALSE
) {

  # 0. Validate data
  plot_shape <- match.arg(plot_shape)
  if (!inherits(data, "data.frame")) cli::cli_abort("`data` must be a data frame")

  diameter_sym <- rlang::enquo(diameter)
  height_sym   <- rlang::enquo(height)
  volume_sym   <- rlang::enquo(volume)
  biomass_sym  <- rlang::enquo(biomass)
  carbon_sym   <- rlang::enquo(carbon)
  province_sym <- rlang::enquo(province)
  species_sym  <- rlang::enquo(species)

  has_vol  <- !rlang::quo_is_null(volume_sym) || predict_volume
  has_bio  <- !rlang::quo_is_null(biomass_sym) || predict_biomass
  has_carb <- !rlang::quo_is_null(carbon_sym) || predict_carbon

  df_work <- data

  groups_vec <- if (is.null(.groups)) character() else .groups

  # Optional on-the-fly predictions
  if (predict_volume) {
    if (rlang::quo_is_null(species_sym) || rlang::quo_is_null(province_sym)) {
      cli::cli_abort("{.arg species} and {.arg province} must be provided when {.arg predict_volume} = TRUE.")
    }
    sp_vals   <- dplyr::pull(df_work, !!species_sym)
    prov_vals <- if (rlang::as_name(province_sym) %in% names(df_work)) {
      dplyr::pull(df_work, !!province_sym)
    } else {
      rlang::eval_tidy(province_sym)
    }
    d_vals <- dplyr::pull(df_work, !!diameter_sym)
    h_vals <- dplyr::pull(df_work, !!height_sym)
    vol_res <- silv_predict_snfi_volume(province = prov_vals, species = sp_vals, dbh = d_vals, h = h_vals, quiet = TRUE)
    df_work$.vol_tree_m3 <- vol_res$vcc / 1000
  } else if (!rlang::quo_is_null(volume_sym)) {
    vol_scale <- if (volume_units == "dm3") 1000 else 1
    df_work$.vol_tree_m3 <- dplyr::pull(df_work, !!volume_sym) / vol_scale
  }

  if (predict_biomass || predict_carbon) {
    if (rlang::quo_is_null(species_sym)) {
      cli::cli_abort("{.arg species} must be provided when predicting biomass or carbon.")
    }
    sp_vals <- dplyr::pull(df_work, !!species_sym)
    if (is.numeric(sp_vals) || is.integer(sp_vals)) {
      snfi_sp <- silv_snfi_species("SNFI4")
      sp_names <- snfi_sp$species_name[match(sp_vals, snfi_sp$species_code)]
      sp_bio_vals <- ifelse(!is.na(sp_names), sp_names, as.character(sp_vals))
    } else {
      sp_bio_vals <- as.character(sp_vals)
    }
    d_vals  <- dplyr::pull(df_work, !!diameter_sym)
    h_vals  <- dplyr::pull(df_work, !!height_sym)
    bio_res <- silv_predict_biomass_auto(species = sp_bio_vals, diameter = d_vals, height = h_vals, component = biomass_component, quiet = TRUE)
    df_work$.bio_tree_t <- bio_res$biomass / 1000

    if (predict_carbon) {
      carb_res <- silv_predict_carbon_auto(biomass = bio_res$biomass, species = sp_bio_vals, component = biomass_component, quiet = TRUE)
      df_work$.carb_tree_t <- carb_res$carbon / 1000
    }
  } else {
    if (!rlang::quo_is_null(biomass_sym)) {
      df_work$.bio_tree_t <- dplyr::pull(df_work, !!biomass_sym) / 1000
    }
    if (!rlang::quo_is_null(carbon_sym)) {
      df_work$.carb_tree_t <- dplyr::pull(df_work, !!carbon_sym) / 1000
    }
  }

  # Calculate .expan per tree
  if (plot_shape == "snfi") {
    df_work <- dplyr::mutate(df_work, .expan = silv_tree_expansion_factor(type = "snfi", diameter = !!diameter_sym))
  } else {
    if (is.null(plot_size)) cli::cli_abort("{.arg plot_size} must be provided for fixed area plots.")
    df_work <- dplyr::mutate(df_work, .expan = silv_density_ntrees_ha(ntrees = 1, plot_size = plot_size, plot_shape = plot_shape))
  }

  # 1. Calculate metrics by dclass
  dclass_by <- if (length(groups_vec) > 0) c(groups_vec, "dclass") else "dclass"
  dclass_group_metrics <- df_work |>
    dplyr::mutate(
      dclass = silv_tree_dclass(!!diameter_sym, dmin, dmax, class_length, include_lowest)
    ) |>
    dplyr::summarise(
      height = mean(!!height_sym, na.rm = TRUE),
      ntrees = dplyr::n(),
      ntrees_ha = sum(.expan, na.rm = TRUE),
      v_tree = if (has_vol) ifelse(all(is.na(.vol_tree_m3)), NA_real_, mean(.vol_tree_m3, na.rm = TRUE)) else NULL,
      w_tree = if (has_bio) ifelse(all(is.na(.bio_tree_t)), NA_real_, mean(.bio_tree_t, na.rm = TRUE)) else NULL,
      c_tree = if (has_carb) ifelse(all(is.na(.carb_tree_t)), NA_real_, mean(.carb_tree_t, na.rm = TRUE)) else NULL,
      .by    = dplyr::all_of(dclass_by)
    ) |>
    dplyr::mutate(
      h0        = silv_stand_dominant_height(dclass, height, ntrees_ha, which = which_h0),
      dg        = silv_stand_qmean_diameter(dclass, ntrees_ha),
      g_ha      = silv_stand_basal_area(dclass, ntrees_ha),
      v_ha      = if (has_vol) v_tree * ntrees_ha else NULL,
      w_ha      = if (has_bio) w_tree * ntrees_ha else NULL,
      c_ha      = if (has_carb) c_tree * ntrees_ha else NULL,
      .by       = if (length(groups_vec) > 0) dplyr::all_of(groups_vec) else NULL
    ) |>
    dplyr::select(-dplyr::any_of(c("v_tree", "w_tree", "c_tree"))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(groups_vec)))

  # 2. Group metrics
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
      v_ha      = if (has_vol) ifelse(all(is.na(v_ha)), NA_real_, sum(v_ha, na.rm = TRUE)) else NULL,
      w_ha      = if (has_bio) ifelse(all(is.na(w_ha)), NA_real_, sum(w_ha, na.rm = TRUE)) else NULL,
      c_ha      = if (has_carb) ifelse(all(is.na(c_ha)), NA_real_, sum(c_ha, na.rm = TRUE)) else NULL,
      .by       = dplyr::all_of(c(groups_vec, "h0", "dg"))
    ) |>
    dplyr::mutate(
      spacing     = silv_density_hart(h0, ntrees_ha, which = which_spacing),
      slenderness = silv_stand_slenderness(h_mean, dg)
    ) |>
    dplyr::select(
      dplyr::all_of(groups_vec), dplyr::starts_with("d_"), dg, dplyr::starts_with("h_"), h0, dplyr::everything()
    )

  # 3. Return an Inventory S7 list
  Inventory(
    dclass_metrics = dclass_group_metrics,
    group_metrics  = groups_metrics,
    groups         = .groups
  )
}


#' Summarize plot data by species
#'
#' Calculates the number of trees, basal area, and optional volume, biomass, and carbon per hectare
#' for each species in a plot, and optionally provides the top species formatted into columns.
#'
#' @param data A data frame or tibble of tree-level data
#' @param plot_id Unquoted column name with the plot identifier
#' @param species Unquoted column name with the species identifier
#' @param expan Unquoted column name with the expansion factor (trees/ha). If `NULL`, it is assumed each row represents 1 tree/ha.
#' @param g Unquoted column name with the basal area per tree (m²/ha). If `NULL`, `diameter` must be provided.
#' @param diameter Unquoted column name with the diameter (cm). Used to calculate basal area if `g` is `NULL`.
#' @param volume Unquoted column name with individual tree volume, optional.
#' @param volume_units Character. Units of the individual tree volume (`"dm3"` or `"m3"`). Default is `"dm3"`.
#' @param biomass Unquoted column name with individual tree biomass (kg), optional.
#' @param carbon Unquoted column name with individual tree carbon (kg), optional.
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
silv_summary_species <- function(
    data,
    plot_id,
    species,
    expan        = NULL,
    g            = NULL,
    diameter     = NULL,
    volume       = NULL,
    volume_units = "dm3",
    biomass      = NULL,
    carbon       = NULL,
    top_n        = 3
) {
  
  if (!inherits(data, "data.frame")) cli::cli_abort("`data` must be a data frame")
  
  plot_id_sym  <- rlang::enquo(plot_id)
  species_sym  <- rlang::enquo(species)
  expan_sym    <- rlang::enquo(expan)
  g_sym        <- rlang::enquo(g)
  diameter_sym <- rlang::enquo(diameter)
  volume_sym   <- rlang::enquo(volume)
  biomass_sym  <- rlang::enquo(biomass)
  carbon_sym   <- rlang::enquo(carbon)
  
  has_vol  <- !rlang::quo_is_null(volume_sym)
  has_bio  <- !rlang::quo_is_null(biomass_sym)
  has_carb <- !rlang::quo_is_null(carbon_sym)

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

  if (has_vol) {
    vol_scale <- if (volume_units == "dm3") 1000 else 1
    data <- dplyr::mutate(data, .v = (!!volume_sym / vol_scale) * .expan)
  }
  if (has_bio) {
    data <- dplyr::mutate(data, .w = (!!biomass_sym / 1000) * .expan)
  }
  if (has_carb) {
    data <- dplyr::mutate(data, .c = (!!carbon_sym / 1000) * .expan)
  }
  
  res <- data |>
    dplyr::group_by(!!plot_id_sym, !!species_sym) |>
    dplyr::summarise(
      N_sp = sum(.expan, na.rm = TRUE),
      G_sp = sum(.g, na.rm = TRUE),
      V_sp = if (has_vol) sum(.v, na.rm = TRUE) else NULL,
      W_sp = if (has_bio) sum(.w, na.rm = TRUE) else NULL,
      C_sp = if (has_carb) sum(.c, na.rm = TRUE) else NULL,
      .groups = "drop"
    ) |>
    dplyr::arrange(!!plot_id_sym, dplyr::desc(G_sp))
    
  if (top_n > 0) {
    if (!requireNamespace("tidyr", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg tidyr} is required for pivoting. Please install it or set {.arg top_n} = 0.")
    }
    
    val_cols <- c(rlang::as_name(species_sym), "G_sp", "N_sp")
    if (has_vol) val_cols <- c(val_cols, "V_sp")
    if (has_bio) val_cols <- c(val_cols, "W_sp")
    if (has_carb) val_cols <- c(val_cols, "C_sp")

    res_pivot <- res |>
      dplyr::group_by(!!plot_id_sym) |>
      dplyr::mutate(.rank = dplyr::row_number()) |>
      dplyr::filter(.rank <= top_n) |>
      dplyr::ungroup()
      
    res_sp <- res_pivot |>
      tidyr::pivot_wider(
        id_cols = !!plot_id_sym,
        names_from = .rank,
        values_from = dplyr::all_of(val_cols)
      )
      
    sp_col_name <- rlang::as_name(species_sym)
    names(res_sp) <- gsub(paste0("^", sp_col_name, "_"), "sp", names(res_sp))
    names(res_sp) <- gsub("_sp_", "_sp", names(res_sp))
    return(res_sp)
  }
  
  return(res)
}


#' Summarize plot data by mortality status
#'
#' Calculates the number of trees, basal area, and optional volume, biomass, and carbon per hectare
#' for live and dead trees in a plot.
#'
#' @param data A data frame or tibble of tree-level data
#' @param plot_id Unquoted column name with the plot identifier
#' @param dead Unquoted column name indicating if the tree is dead (e.g., TRUE/FALSE or 1/0)
#' @param expan Unquoted column name with the expansion factor (trees/ha). If `NULL`, it is assumed each row represents 1 tree/ha.
#' @param g Unquoted column name with the basal area per tree (m²/ha). If `NULL`, `diameter` must be provided.
#' @param diameter Unquoted column name with the diameter (cm). Used to calculate basal area if `g` is `NULL`.
#' @param volume Unquoted column name with individual tree volume, optional.
#' @param volume_units Character. Units of the individual tree volume (`"dm3"` or `"m3"`). Default is `"dm3"`.
#' @param biomass Unquoted column name with individual tree biomass (kg), optional.
#' @param carbon Unquoted column name with individual tree carbon (kg), optional.
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
silv_summary_mortality <- function(
    data,
    plot_id,
    dead,
    expan        = NULL,
    g            = NULL,
    diameter     = NULL,
    volume       = NULL,
    volume_units = "dm3",
    biomass      = NULL,
    carbon       = NULL
) {
  
  if (!inherits(data, "data.frame")) cli::cli_abort("`data` must be a data frame")
  
  plot_id_sym  <- rlang::enquo(plot_id)
  dead_sym     <- rlang::enquo(dead)
  expan_sym    <- rlang::enquo(expan)
  g_sym        <- rlang::enquo(g)
  diameter_sym <- rlang::enquo(diameter)
  volume_sym   <- rlang::enquo(volume)
  biomass_sym  <- rlang::enquo(biomass)
  carbon_sym   <- rlang::enquo(carbon)
  
  has_vol  <- !rlang::quo_is_null(volume_sym)
  has_bio  <- !rlang::quo_is_null(biomass_sym)
  has_carb <- !rlang::quo_is_null(carbon_sym)

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

  if (has_vol) {
    vol_scale <- if (volume_units == "dm3") 1000 else 1
    data <- dplyr::mutate(data, .v = (!!volume_sym / vol_scale) * .expan)
  }
  if (has_bio) {
    data <- dplyr::mutate(data, .w = (!!biomass_sym / 1000) * .expan)
  }
  if (has_carb) {
    data <- dplyr::mutate(data, .c = (!!carbon_sym / 1000) * .expan)
  }
  
  res <- data |>
    dplyr::mutate(.status = ifelse(!!dead_sym, "dead", "alive")) |>
    dplyr::group_by(!!plot_id_sym) |>
    dplyr::summarise(
      N_alive = sum(.expan[.status == "alive"], na.rm = TRUE),
      N_dead  = sum(.expan[.status == "dead"], na.rm = TRUE),
      G_alive = sum(.g[.status == "alive"], na.rm = TRUE),
      G_dead  = sum(.g[.status == "dead"], na.rm = TRUE),
      V_alive = if (has_vol) sum(.v[.status == "alive"], na.rm = TRUE) else NULL,
      V_dead  = if (has_vol) sum(.v[.status == "dead"], na.rm = TRUE) else NULL,
      W_alive = if (has_bio) sum(.w[.status == "alive"], na.rm = TRUE) else NULL,
      W_dead  = if (has_bio) sum(.w[.status == "dead"], na.rm = TRUE) else NULL,
      C_alive = if (has_carb) sum(.c[.status == "alive"], na.rm = TRUE) else NULL,
      C_dead  = if (has_carb) sum(.c[.status == "dead"], na.rm = TRUE) else NULL,
      .groups = "drop"
    )
    
  return(res)
}

