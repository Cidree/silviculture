#' Predict carbon content from biomass
#'
#' @description
#' `silv_predict_carbon()` calculates the carbon content of a tree component based on its 
#' predicted biomass. It uses species- and component-specific carbon percentages 
#' from the package's internal `carbon_models` database.
#' 
#' `silv_predict_carbon_auto()` is a vectorized function that automatically selects 
#' the best available carbon model for each row based on a provided priority list. 
#' If the exact species is not found, it attempts a genus-level fallback (e.g., "Pinus spp.").
#'
#' @param biomass A numeric vector of predicted biomass (in kg).
#' @param species A character vector of tree species (e.g., `"Pinus pinaster"`).
#' @param component A character string or vector specifying the tree component 
#'   (e.g., `"stem"`, `"roots"`, `"tree"`). Must match the `tree_component` column 
#'   in the [carbon_models] dataset. Note that some models do not provide carbon 
#'   percentages for aggregate groups like `"AGB"`.
#' @param model A character string specifying the carbon model to use (e.g., `"montero-2005"`).
#' @param priority A character vector specifying the order of preference for carbon models.
#'   Default is `c("montero-2005", "dieguez-aranda-2009")`.
#' @param quiet Logical. If `FALSE`, informs the user about genus fallbacks or missing species.
#'
#' @return 
#' For `silv_predict_carbon()`: A numeric vector of predicted carbon (in kg). 
#' Returns `NA` if the species/component combination is not supported by the model.
#' 
#' For `silv_predict_carbon_auto()`: A `data.frame` with two columns:
#'   - `carbon`: The predicted carbon (in kg).
#'   - `carbon_model`: The model and species level used (e.g., `"montero-2005"` or `"montero-2005 (genus fallback)"`).
#'
#' @name predict_carbon
NULL

#' @rdname predict_carbon
#' @export
silv_predict_carbon <- function(biomass, species, component, model = "montero-2005", quiet = FALSE) {
  # Validations
  n_trees <- length(biomass)
  if (length(species) != n_trees) {
    cli::cli_abort("{.arg biomass} and {.arg species} must have the same length.")
  }
  if (length(component) == 1) {
    component <- rep(component, n_trees)
  } else if (length(component) != n_trees) {
    cli::cli_abort("{.arg component} must be of length 1 or the same length as {.arg biomass}.")
  }

  carbon_values <- rep(NA_real_, n_trees)
  
  # Predict element by element (or grouped by species-component if we want to vectorize)
  unique_combos <- unique(data.frame(species = species, component = component, stringsAsFactors = FALSE))
  
  for (i in seq_len(nrow(unique_combos))) {
    sp <- unique_combos$species[i]
    comp <- unique_combos$component[i]
    
    # In case component comes capitalized (like AGB, BGB from previous step)
    comp_lower <- tolower(comp)
    
    pct <- .lookup_carbon_pct(sp, comp_lower, model)
    
    idx <- which(species == sp & component == comp)
    
    if (is.na(pct)) {
      if (!quiet) {
        cli::cli_alert_warning("Model {.val {model}} does not support species {.val {sp}} and component {.val {comp}}.")
      }
    } else {
      carbon_values[idx] <- biomass[idx] * (pct / 100)
    }
  }
  
  return(carbon_values)
}

#' @rdname predict_carbon
#' @export
silv_predict_carbon_auto <- function(biomass, species, component, priority = c("montero-2005", "dieguez-aranda-2009"), quiet = FALSE) {
  n_trees <- length(biomass)
  if (length(species) != n_trees) {
    cli::cli_abort("{.arg biomass} and {.arg species} must have the same length.")
  }
  if (length(component) == 1) {
    component <- rep(component, n_trees)
  } else if (length(component) != n_trees) {
    cli::cli_abort("{.arg component} must be of length 1 or the same length as {.arg biomass}.")
  }
  
  carbon_values <- rep(NA_real_, n_trees)
  carbon_models_used <- rep(NA_character_, n_trees)
  
  unique_combos <- unique(data.frame(species = species, component = component, stringsAsFactors = FALSE))
  
  for (i in seq_len(nrow(unique_combos))) {
    sp <- unique_combos$species[i]
    comp <- unique_combos$component[i]
    
    idx <- which(species == sp & component == comp)
    
    comp_lower <- tolower(comp)
    
    best_model_info <- .auto_select_carbon_model(sp, comp_lower, priority)
    
    if (is.na(best_model_info$model)) {
      if (!quiet) {
        cli::cli_alert_warning(
          "Could not find carbon percentage for {.val {sp}} (component: {.val {comp}}).
          Consider manually assigning a generic group like {.val Otras con\u00edferas}, {.val Otras frondosas}, or {.val Otras laurisilvas}."
        )
      }
    } else {
      # Show info if fallback was used
      if (best_model_info$is_fallback && !quiet) {
        cli::cli_alert_info("Exact species {.val {sp}} not found. Using genus fallback {.val {best_model_info$matched_species}} in model {.val {best_model_info$model}}.")
      }
      
      carbon_values[idx] <- biomass[idx] * (best_model_info$pct / 100)
      
      if (best_model_info$is_fallback) {
        carbon_models_used[idx] <- paste0(best_model_info$model, " (genus fallback)")
      } else {
        carbon_models_used[idx] <- best_model_info$model
      }
    }
  }
  
  return(data.frame(
    carbon = carbon_values,
    carbon_model = carbon_models_used,
    stringsAsFactors = FALSE
  ))
}


# --- Internal Helpers ---

.lookup_carbon_pct <- function(species, component, model) {
  sel <- silviculture::carbon_models[
    silviculture::carbon_models$article_id == model & 
    silviculture::carbon_models$species == species & 
    silviculture::carbon_models$tree_component == component, 
  ]
  
  if (nrow(sel) == 0) return(NA_real_)
  
  # Some might have duplicate entries (though shouldn't), take first
  return(sel$carbon_percentage[1])
}

.auto_select_carbon_model <- function(species, component, priority) {
  
  # 1. Try exact species
  for (mod in priority) {
    pct <- .lookup_carbon_pct(species, component, mod)
    if (!is.na(pct)) {
      return(list(model = mod, pct = pct, matched_species = species, is_fallback = FALSE))
    }
  }
  
  # 2. Try genus fallback
  genus <- strsplit(species, " ")[[1]][1]
  genus_spp <- paste0(genus, " spp.")
  
  for (mod in priority) {
    pct <- .lookup_carbon_pct(genus_spp, component, mod)
    if (!is.na(pct)) {
      return(list(model = mod, pct = pct, matched_species = genus_spp, is_fallback = TRUE))
    }
  }
  
  # 3. Not found
  return(list(model = NA_character_, pct = NA_real_, matched_species = NA_character_, is_fallback = FALSE))
}
