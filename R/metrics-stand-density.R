#' Calculates number of trees per hectare
#'
#' Calculates number of trees per hectare for a given plot size and shape
#'
#' @param ntrees A numeric vector representing the number of trees in a sampling plot
#' @template plot_size
#' @template plot_shape
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
#'     ntrees_ha = silv_density_ntrees_ha(n, plot_size = 10)
#'   )
#'
#' ## Rectangular plot of 10x15 meters
#' inventory_samples |>
#'   count(plot_id, species) |>
#'   mutate(
#'     ntrees_ha = silv_density_ntrees_ha(
#'       n,
#'       plot_size = c(10, 15),
#'       plot_shape = "rectangular"
#'     )
#'   )
silv_density_ntrees_ha <- function(ntrees,
                                   plot_size,
                                   plot_shape = "circular") {
  # 0. Handle errors
  stopifnot(plot_shape %in% c("circular", "rectangular"))
  if (length(plot_size) == 1 && plot_size <= 0) cli::cli_abort("`plot_size` has to be greater than 0")

  # 1. Calculate ntrees in ha
  if (plot_shape == "circular") {
    ntrees * 10000 / (pi * plot_size**2)
  } else {
    ntrees * 10000 / prod(plot_size)
  }
}


## https://www.fs.usda.gov/Internet/FSE_DOCUMENTS/stelprdb5270993.pdf
## https://academic.oup.com/forestry/article-abstract/85/1/27/643571?redirectedFrom=fulltext&login=false

#' Calculates the Stand Density Index
#'
#' The Stand Density Index (SDI) is the relationship between the average tree size and
#' density of trees per hectare.
#'
#' @template ntrees
#' @template dg
#' @param beta The Stand Density Index exponent (default is \code{1.605}).
#'
#' @return A numeric vector representing the absolute SDI.
#' @export
#'
#' @details
#' The SDI has different interpretations depending on the species, location, and also
#' the management type (even-aged, uneven-aged...). The value of maximum SDI must
#' be determined from the literature and used carefully. The \code{beta} exponent allows
#' adjustments for different species or mixed stands.
#'
#' @references Reineke, L. H. (1933). Perfecting a stand-density index for even-aged forests.
#'   Journal of Agricultural Research, 46(7), 627-638. URL: https://research.fs.usda.gov/download/treesearch/60134.pdf
#'
#' @examples
#' ## calculate SDI for a Pinus sylvestris stand (beta = 1.605)
#' silv_density_sdi(ntrees = 800, dg = 23.4)
#'
#' ## calculate SDI with custom beta
#' silv_density_sdi(ntrees = 800, dg = 23.4, beta = 1.7)
silv_density_sdi <- function(
  ntrees,
  dg,
  beta = 1.605
) {
  # 0. validate inputs
  assert_positive_numeric(ntrees, "ntrees")
  assert_positive_numeric(dg, "dg")
  if (!is.numeric(beta)) cli::cli_abort("{.arg beta} has to be a numeric vector.")
  assert_same_length(ntrees, dg, names = c("ntrees", "dg"))

  # 1. calculate sdi
  sdi <- ntrees * ((25.4 / dg)**-abs(beta)) # note: abs() avoids errors with signs
  return(sdi)
}

# --- Internal Auto Selector Helper ---

#' @noRd
.auto_select_sdi_beta <- function(species, region = NULL) {
  
  sdi_models <- silviculture::sdi_models
  
  # 1. Try exact species + region match
  if (!is.null(region)) {
    sel <- sdi_models[sdi_models$species == species & sdi_models$region == region, ]
    if (nrow(sel) > 0) {
      return(list(
        model = sel$article_id[1], 
        beta = sel$beta[1], 
        matched_species = species, 
        matched_region = region,
        is_fallback = FALSE,
        fallback_type = NA_character_,
        model_desc = paste0(sel$article_id[1], " (", region, ")")
      ))
    }
  }
  
  # 2. Try species + "all" regions fallback
  sel <- sdi_models[sdi_models$species == species & sdi_models$region == "all", ]
  if (nrow(sel) > 0) {
    return(list(
      model = sel$article_id[1], 
      beta = sel$beta[1], 
      matched_species = species, 
      matched_region = "all",
      is_fallback = !is.null(region),
      fallback_type = "region",
      model_desc = paste0(sel$article_id[1], " (all regions)")
    ))
  }
  
  # 3. Try genus fallback (genus spp.)
  genus <- strsplit(species, " ")[[1]][1]
  genus_spp <- paste0(genus, " spp.")
  
  if (!is.null(region)) {
    sel <- sdi_models[sdi_models$species == genus_spp & sdi_models$region == region, ]
    if (nrow(sel) > 0) {
      return(list(
        model = sel$article_id[1], 
        beta = sel$beta[1], 
        matched_species = genus_spp, 
        matched_region = region,
        is_fallback = TRUE,
        fallback_type = "genus",
        model_desc = paste0(sel$article_id[1], " (genus fallback)")
      ))
    }
  }
  
  sel <- sdi_models[sdi_models$species == genus_spp & sdi_models$region == "all", ]
  if (nrow(sel) > 0) {
    return(list(
      model = sel$article_id[1], 
      beta = sel$beta[1], 
      matched_species = genus_spp, 
      matched_region = "all",
      is_fallback = TRUE,
      fallback_type = "genus",
      model_desc = paste0(sel$article_id[1], " (genus fallback)")
    ))
  }
  
  # 4. Total fallback (default)
  sel <- sdi_models[sdi_models$species == "default", ]
  if (nrow(sel) == 0) {
    # Absolute safety fallback in case dataset is malformed
    default_beta <- 1.605
  } else {
    default_beta <- sel$beta[1]
  }
  
  return(list(
    model = "default", 
    beta = default_beta, 
    matched_species = "default", 
    matched_region = "default",
    is_fallback = TRUE,
    fallback_type = "default",
    model_desc = paste0("default (", default_beta, ")")
  ))
}

#' Predict Stand Density Index automatically
#'
#' @description
#' `silv_density_sdi_auto()` is a vectorized function that automatically selects
#' the best available Stand Density Index exponent (\code{beta}) for each row
#' based on a provided species and region from the internal \code{sdi_models} database.
#' 
#' If an exact species and region match is not found, the function falls back to a 
#' country-wide species model (\code{region = "all"}), then to a genus-level fallback 
#' (e.g., "Pinus spp."), and finally to the default SDI exponent (\code{beta = 1.605}).
#'
#' @template ntrees
#' @template dg
#' @param species A character string or vector of tree species (e.g., `"Pinus sylvestris"`).
#' @param region A character string or vector of the region (e.g., `"Castilla y Leon"`).
#'   Defaults to `NULL` (no region specified).
#' @param quiet Logical. If `FALSE`, informs the user about fallbacks to genus or default models.
#'
#' @return A `data.frame` with two columns:
#'   - `sdi`: The computed absolute Stand Density Index.
#'   - `sdi_model`: The model used (e.g., `"del-rio-2006 (Castilla y Leon)"`, 
#'     `"aguirre-2017 (genus fallback)"`, or `"default (1.605)"`).
#'
#' @name silv_density_sdi_auto
#'
#' @examples
#' # Calculate SDI with automatic selection
#' silv_density_sdi_auto(
#'   ntrees = 800,
#'   dg = 23.4,
#'   species = "Pinus sylvestris",
#'   region = "Castilla y Leon"
#' )
#'
#' # Fallback to default
#' silv_density_sdi_auto(
#'   ntrees = 800,
#'   dg = 23.4,
#'   species = "Unknown species"
#' )
#'
#' @export
silv_density_sdi_auto <- function(
  ntrees,
  dg,
  species,
  region = NULL,
  quiet = FALSE
) {
  # Validations
  n_trees <- length(ntrees)
  assert_positive_numeric(ntrees, "ntrees")
  assert_positive_numeric(dg, "dg")
  assert_same_length(ntrees, dg, names = c("ntrees", "dg"))

  if (length(species) == 1) {
    species <- rep(species, n_trees)
  } else if (length(species) != n_trees) {
    cli::cli_abort("{.arg species} must be of length 1 or the same length as {.arg ntrees}.")
  }

  if (is.null(region)) {
    region <- rep(NA_character_, n_trees)
  } else if (length(region) == 1) {
    region <- rep(region, n_trees)
  } else if (length(region) != n_trees) {
    cli::cli_abort("{.arg region} must be of length 1 or the same length as {.arg ntrees}.")
  }

  sdi_values <- rep(NA_real_, n_trees)
  sdi_models_used <- rep(NA_character_, n_trees)
  
  unique_combos <- unique(data.frame(
    species = species, 
    region = region, 
    stringsAsFactors = FALSE
  ))
  
  for (i in seq_len(nrow(unique_combos))) {
    sp <- unique_combos$species[i]
    reg <- unique_combos$region[i]
    
    # Handle NA in region
    if (is.na(reg)) {
      idx <- which(species == sp & is.na(region))
      reg_arg <- NULL
    } else {
      idx <- which(species == sp & region == reg)
      reg_arg <- reg
    }
    
    best_model_info <- .auto_select_sdi_beta(sp, reg_arg)
    
    if (best_model_info$is_fallback && !quiet) {
      if (best_model_info$fallback_type == "default") {
         cli::cli_alert_info("Exact model for {.val {sp}} not found. Using default beta {.val {best_model_info$beta}}.")
      } else if (best_model_info$fallback_type == "genus") {
         cli::cli_alert_info("Exact species {.val {sp}} not found. Using genus fallback {.val {best_model_info$matched_species}} from {.val {best_model_info$model}}.")
      } else if (best_model_info$fallback_type == "region") {
         cli::cli_alert_info("Exact region {.val {reg_arg}} not found for {.val {sp}}. Using fallback region {.val {best_model_info$matched_region}} from {.val {best_model_info$model}}.")
      }
    }
    
    sdi_values[idx] <- silv_density_sdi(ntrees[idx], dg[idx], beta = best_model_info$beta)
    sdi_models_used[idx] <- best_model_info$model_desc
  }
  
  return(data.frame(
    sdi = sdi_values,
    sdi_model = sdi_models_used,
    stringsAsFactors = FALSE
  ))
}


#' Classifies the Stand Density Index
#'
#' Classifies the Stand Density Index (SDI) into density classes or calculates the relative SDI
#' percentage based on USDA thresholds.
#'
#' @param sdi A numeric vector representing the Stand Density Index.
#' @param max_sdi A numeric vector representing the maximum SDI for the species/site.
#' @param classify A logical value indicating whether to classify the values into density classes
#'   (default is \code{TRUE}). If \code{FALSE}, it returns the relative SDI as a percentage.
#'
#' @return A character vector with the density classes if \code{classify = TRUE}, or a numeric vector
#'   with the relative SDI percentage if \code{classify = FALSE}.
#' @export
#'
#' @details
#' The option \code{classify = TRUE} will use the \code{max_sdi} value to classify the SDI into
#' four competitive and growth conditions: low density (<24%), moderate density (24-35%),
#' high density (34-55%), and extremely high density (>55%).
#'
#' @references USDA Forest Service. (n.d.). Stand Density Index.
#'   https://www.fs.usda.gov/Internet/FSE_DOCUMENTS/stelprdb5270993.pdf
#'
#' @examples
#' ## calculate SDI for a Pinus sylvestris stand (max 990)
#' sdi_val <- silv_density_sdi(ntrees = 800, dg = 23.4)
#'
#' ## check base classification
#' silv_density_sdi_class(sdi = sdi_val, max_sdi = 990)
#'
#' ## get relative SDI percentage
#' silv_density_sdi_class(sdi = sdi_val, max_sdi = 990, classify = FALSE)
silv_density_sdi_class <- function(
  sdi,
  max_sdi,
  classify = TRUE
) {
  # 0. validate inputs
  assert_positive_numeric(sdi, "sdi")
  assert_positive_numeric(max_sdi, "max_sdi")
  assert_logical(classify, "classify")
  assert_same_length(sdi, max_sdi, names = c("sdi", "max_sdi"))

  # 1. calculate relative sdi
  rel_sdi <- (sdi / max_sdi) * 100

  # 2. classify or return percentage
  if (classify) {
    res <- dplyr::case_when(
      rel_sdi <= 24 ~ "Low density",
      rel_sdi > 24 & rel_sdi <= 34 ~ "Moderate density",
      rel_sdi > 34 & rel_sdi <= 55 ~ "High density",
      rel_sdi > 55 ~ "Extremely high density"
    )
  } else {
    res <- rel_sdi
  }
  return(res)
}


#' Hart or Hart-Becking spacing index
#'
#' Calculates the Hart Index or the Hart-Becking Index for even-aged stands
#'
#' @template h0
#' @template ntrees
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
#'     h0     = silv_stand_dominant_height(diameter, height),
#'     ntrees = n(),
#'     .by    = plot_id
#'   ) |>
#'   ## calculate number of trees per hectare
#'   mutate(ntrees_ha = silv_density_ntrees_ha(ntrees, plot_size = 14.1)) |>
#'   mutate(spacing = silv_density_hart(h0, ntrees_ha))
silv_density_hart <- function(
  h0,
  ntrees,
  which = c("hart", "hart-becking")
) {
  # 0. Validate inputs
  assert_positive_numeric(h0, "h0")
  assert_positive_numeric(ntrees, "ntrees")
  assert_same_length(h0, ntrees, names = c("h0", "ntrees"))
  dh_method <- match.arg(which)

  # 1. Calculate spacing index
  switch(dh_method,
    "hart"         = 10000 / h0 / sqrt(ntrees),
    "hart-becking" = sqrt(20000 / (ntrees * sqrt(3))) / h0 * 100,
    cli::cli_abort("`which` must be either <hart> or <hart-becking>")
  )
}
