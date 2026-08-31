#' Tree-level inventory summary
#'
#' Computes individual tree-level dendrometric metrics, diametric classes,
#' expansion factors, competition indices, and optional volume, biomass, and carbon predictions.
#'
#' @param data A data frame or tibble with tree-level records.
#' @param diameter Unquoted column name with the tree diameter (in cm).
#' @param height Unquoted column name with the tree height (in m), optional.
#' @param plot_id Unquoted column name with the plot identifier, optional.
#' @param species Unquoted column name with the tree species identifier/name, optional.
#' @param expan Unquoted column name with the expansion factor (trees/ha), optional.
#' @param plot_size Numeric. Size of the sampling plot (radius in meters if circular, area in m² if rectangular).
#' @param plot_shape Character. Shape of the sampling plot (`"circular"` or `"rectangular"`). Default is `"circular"`.
#' @param dmin Numeric. Minimum diameter for diametric classes (default: 7.5).
#' @param dmax Numeric. Maximum diameter for diametric classes (default: NULL).
#' @param class_length Numeric. Width of diametric classes (default: 5).
#' @param include_lowest Logical. Whether to include lowest bound in classes (default: TRUE).
#' @param compute_bal Logical. If TRUE and `plot_id` is supplied, computes BAL and BAS (default: FALSE).
#' @param predict_volume Logical. If TRUE, predicts SNFI volume (vcc, vsc, iavc) using [silv_predict_snfi_volume()] (default: FALSE).
#' @param province Unquoted column name or scalar string/integer with the province code/name for volume prediction.
#' @param predict_biomass Logical. If TRUE, predicts tree biomass using [silv_predict_biomass_auto()] (default: FALSE).
#' @param biomass_component Character. Tree component to predict for biomass (default: `"tree"`).
#' @param predict_carbon Logical. If TRUE, predicts tree carbon content using [silv_predict_carbon_auto()] (default: FALSE).
#'
#' @return The original data frame enriched with computed tree-level metrics.
#' @export
#'
#' @examples
#' library(dplyr)
#' silv_tree_summary(
#'   data       = inventory_samples,
#'   diameter   = diameter,
#'   height     = height,
#'   plot_id    = plot_id,
#'   species    = species,
#'   plot_size  = 10,
#'   compute_bal = TRUE
#' )
silv_tree_summary <- function(
    data,
    diameter,
    height           = NULL,
    plot_id          = NULL,
    species          = NULL,
    expan            = NULL,
    plot_size        = NULL,
    plot_shape       = c("circular", "rectangular", "snfi"),
    dmin             = 7.5,
    dmax             = NULL,
    class_length     = 5,
    include_lowest   = TRUE,
    compute_bal      = FALSE,
    predict_volume   = FALSE,
    province         = NULL,
    predict_biomass  = FALSE,
    biomass_component = "tree",
    predict_carbon   = FALSE
) {
  # 0. Validate input data
  plot_shape <- match.arg(plot_shape)
  if (!inherits(data, "data.frame")) {
    cli::cli_abort("`data` must be a data frame or tibble.")
  }

  diameter_sym <- rlang::enquo(diameter)
  height_sym   <- rlang::enquo(height)
  plot_id_sym  <- rlang::enquo(plot_id)
  species_sym  <- rlang::enquo(species)
  expan_sym    <- rlang::enquo(expan)
  province_sym <- rlang::enquo(province)

  if (rlang::quo_is_null(diameter_sym)) {
    cli::cli_abort("{.arg diameter} must be specified.")
  }

  res <- data

  # 1. Diametric class and tree basal area
  d_vals <- dplyr::pull(res, !!diameter_sym)
  res <- dplyr::mutate(
    res,
    dclass = silv_tree_dclass(
      diameter       = !!diameter_sym,
      dmin           = dmin,
      dmax           = dmax,
      class_length   = class_length,
      include_lowest = include_lowest
    ),
    g = silv_tree_basal_area(diameter = !!diameter_sym, units = "cm")
  )

  # 2. Expansion factor (expan) and Basal area per hectare (g_ha)
  if (!rlang::quo_is_null(expan_sym)) {
    res <- dplyr::mutate(
      res,
      expan = !!expan_sym,
      g_ha  = silv_tree_basal_area_ha(diameter = !!diameter_sym, expansion_factor = expan, units = "cm")
    )
  } else if (plot_shape == "snfi") {
    res <- dplyr::mutate(
      res,
      expan = silv_tree_expansion_factor(type = "snfi", diameter = !!diameter_sym),
      g_ha  = silv_tree_basal_area_ha(diameter = !!diameter_sym, expansion_factor = expan, units = "cm")
    )
  } else if (!is.null(plot_size)) {
    res <- dplyr::mutate(
      res,
      expan = silv_density_ntrees_ha(ntrees = 1, plot_size = plot_size, plot_shape = plot_shape),
      g_ha  = silv_tree_basal_area_ha(diameter = !!diameter_sym, expansion_factor = expan, units = "cm")
    )
  }

  # 3. Tree slenderness (if height is provided)
  if (!rlang::quo_is_null(height_sym)) {
    res <- dplyr::mutate(
      res,
      slenderness = silv_tree_slenderness(height = !!height_sym, diameter = !!diameter_sym)
    )
  }

  # 4. Competition indices (BAL and BAS)
  if (compute_bal) {
    if (rlang::quo_is_null(plot_id_sym)) {
      cli::cli_abort("{.arg plot_id} must be provided when {.arg compute_bal} = TRUE.")
    }
    res <- res |>
      dplyr::mutate(.temp_tree_id = dplyr::row_number())

    bal_vals <- silv_tree_bal(
      data             = res,
      plot_id          = !!plot_id_sym,
      tree_id          = .temp_tree_id,
      diameter         = !!diameter_sym,
      expansion_factor = if ("expan" %in% names(res)) expan else NULL,
      basal_area_ha    = if ("g_ha" %in% names(res)) g_ha else if (!("expan" %in% names(res))) g else NULL
    )
    bas_vals <- silv_tree_bas(
      data             = res,
      plot_id          = !!plot_id_sym,
      tree_id          = .temp_tree_id,
      diameter         = !!diameter_sym,
      expansion_factor = if ("expan" %in% names(res)) expan else NULL,
      basal_area_ha    = if ("g_ha" %in% names(res)) g_ha else if (!("expan" %in% names(res))) g else NULL
    )
    res <- res |>
      dplyr::mutate(
        bal = bal_vals,
        bas = bas_vals
      ) |>
      dplyr::select(-.temp_tree_id)
  }

  # 5. Optional Volume Prediction
  if (predict_volume) {
    if (rlang::quo_is_null(species_sym)) {
      cli::cli_abort("{.arg species} must be provided when {.arg predict_volume} = TRUE.")
    }
    if (rlang::quo_is_null(province_sym)) {
      cli::cli_abort("{.arg province} must be provided when {.arg predict_volume} = TRUE.")
    }
    sp_vals   <- dplyr::pull(res, !!species_sym)
    prov_vals <- if (rlang::as_name(province_sym) %in% names(res)) {
      dplyr::pull(res, !!province_sym)
    } else {
      rlang::eval_tidy(province_sym)
    }
    h_vals <- if (!rlang::quo_is_null(height_sym)) dplyr::pull(res, !!height_sym) else NULL

    vol_res <- silv_predict_snfi_volume(
      province = prov_vals,
      species  = sp_vals,
      dbh      = d_vals * 10, # mm
      h        = h_vals,
      quiet    = TRUE
    )
    res <- dplyr::bind_cols(res, vol_res)
  }

  # 6. Optional Biomass Prediction
  if (predict_biomass || predict_carbon) {
    if (rlang::quo_is_null(species_sym)) {
      cli::cli_abort("{.arg species} must be provided when {.arg predict_biomass} or {.arg predict_carbon} is TRUE.")
    }
    sp_vals <- dplyr::pull(res, !!species_sym)
    if (is.numeric(sp_vals) || is.integer(sp_vals)) {
      snfi_sp <- silv_snfi_species("SNFI4")
      sp_names <- snfi_sp$species_name[match(sp_vals, snfi_sp$species_code)]
      sp_bio_vals <- ifelse(!is.na(sp_names), sp_names, as.character(sp_vals))
    } else {
      sp_bio_vals <- as.character(sp_vals)
    }
    h_vals  <- if (!rlang::quo_is_null(height_sym)) dplyr::pull(res, !!height_sym) else NULL

    bio_res <- silv_predict_biomass_auto(
      species   = sp_bio_vals,
      diameter  = d_vals,
      height    = h_vals,
      component = biomass_component,
      quiet     = TRUE
    )
    res <- dplyr::bind_cols(res, bio_res)

    # 7. Optional Carbon Prediction
    if (predict_carbon) {
      carb_res <- silv_predict_carbon_auto(
        biomass   = bio_res$biomass,
        species   = sp_bio_vals,
        component = biomass_component,
        quiet     = TRUE
      )
      res <- dplyr::bind_cols(res, carb_res)
    }
  }

  return(res)
}
