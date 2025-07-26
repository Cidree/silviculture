

#' Calculate Tree Biomass
#'
#' Computes the biomass of a tree species using species-specific allometric
#' equations (in kg).
#'
#' @param diameter A numeric vector of tree diameters (in cm).
#' @param height A numeric vector of tree heights (in m).
#' @param ntrees An optional numeric value indicating the number of trees in
#' this diameter-height class. Defaults to 1 if NULL.
#' @param species A character string specifying the scientific name of the tree
#' species. See Details for available species.
#' @param component A character string specifying the tree component for biomass
#' calculation (e.g., "tree", "stem", "branches"). See Details.
#' @param model A character string indicating the ID of the publication in which
#' the model was developed. Currently supported models: "ruiz-peinado-2012"
#' (hardwood species in Spain) and "ruiz-peinado-2011" (softwood species in
#' Spain). See Details.
#' @param return_rmse A logical value. If TRUE, the function returns the root
#' mean squared error (RMSE) of the selected model instead of the biomass value.
#' @param quiet A logical value. If TRUE, suppresses any informational messages.
#'
#' @return A numeric vector of biomass values (in kg). If `return_rmse = TRUE`, returns the RMSE instead.
#'
#' @export
#'
#' @details
#' The function estimates biomass using validated allometric models available in the
#' dataset \link{biomass_models}. The available models include:
#'
#' - **ruiz-peinado-2011**: Developed for softwood species in Spain.
#' - **ruiz-peinado-2012**: Developed for hardwood species in Spain.
#'
#' Users can check the list of supported species and their corresponding components
#' in \link{biomass_models}.
#'
#' If you would like to suggest additional models, please open a new issue on GitHub.
#'
#' @examples
#' # Calculate biomass for a single tree
#' silv_predict_biomass(
#'   diameter = 45,
#'   height   = 22,
#'   species  = "Pinus pinaster",
#'   model    = "ruiz-peinado-2011"
#' )
silv_predict_biomass <- function(
    diameter    = NULL,
    height      = NULL,
    ntrees      = NULL,
    species     = NULL,
    component   = "stem",
    model       = "ruiz-peinado-2012",
    return_rmse = FALSE,
    quiet       = FALSE) {

  # 0. Handle errors and setup
  if (is.null(ntrees)) ntrees <- rep(1, length(diameter))
  if (component == "tree" & return_rmse) cli::cli_abort("RMSE is only available by single components.")

  ## 0.2. Ensure species has same length as the rest (it can be a constant)
  species <- rep_len(species, length(diameter))

  # 1. Define a helper function to calculate biomass for a single tree
  calc_biomass <- function(d, h, n, sp) {
    ## 1.1. Filter model
    sel_model <- biomass_models[biomass_models$article_id == model, ]
    ## 1.2. Filter tree species
    sel_species <- sel_model[sel_model$species == sp, ]
    ## 1.3. Filter component
    sel_component <- sel_species[sel_species$component %in% component, ]

    ## 1.4. Check if there's a matching model
    if (nrow(sel_component) == 0) cli::cli_abort(
      "The combination of species-component-model doesn't match any available option.
      Check {.url https://cidree.github.io/silviculture/reference/biomass_models.html} for available models."
    )
    # if (!quiet) cli::cli_alert_warning("Cite this model using {.url {sel_component$doi}}")
    ## 2. Calculate biomass
    if (grepl("h", sel_component$expression)) {
      f1 <- function(d, h) eval(parse(text = sel_component$expression))
      biomass <- f1(d, h) * n
    } else {
      f2 <- function(d) eval(parse(text = sel_component$expression))
      biomass <- f2(d) * n
    }

    ## create a table with the outputs
    biomass_tbl <- data.frame(
      biomass  = biomass,
      rmse     = sel_component$rmse,
      citation = sel_component$doi
    )

    return(biomass_tbl)
  }

  # 2. Vectorize the function to handle multiple inputs
  biomass_mat <- mapply(calc_biomass, diameter, height, ntrees, species)

  biomass_df <- biomass_mat |>
    t() |>
    data.frame()

  biomass_df[] <- lapply(biomass_df, unlist)

  # 3. Feedback messages
  if (!quiet) {
    dois <- unique(biomass_df$citation)
    if (length(dois) == 1) {
      cli::cli_alert_warning("Cite this model using {.url {dois}}")
    } else {
      cli::cli_alert_warning("Cite these models using <{paste0(dois, collapse = ', ')}>")
    }
  }

  # 4. Return the biomass values
  if (!return_rmse) {
    return(biomass_df$biomass)
  } else {
    return(biomass_df$rmse)
  }
}