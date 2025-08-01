
ModelBiomass <- S7::new_class(
  name    = "ModelBiomass",
  package = "silviculture",
  properties = list(
    equation   = S7::new_property(S7::class_character, default = quote(list())),
    species    = S7::new_property(S7::class_character, default = quote(list())),
    expression = S7::new_property(S7::class_data.frame, default = quote(list())),
    doi        = S7::new_property(S7::class_character, default = quote(list())),
    obs        = S7::new_property(S7::class_character, default = quote(list())),
    params     = S7::new_property(S7::class_list, default = quote(list()))
  )
)





#' Calculate Tree Biomass
#'
#' Computes the biomass of a tree species using species-specific allometric
#' equations (in kg). Currently, only equations for Spain are available.
#'
#' @param diameter A numeric vector of tree diameters (in cm).
#' @param height A numeric vector of tree heights (in m).
#' @param model A function. A function with the structure \code{eq_biomass_*()} with
#' additional arguments depending on the model used.
#' @param ntrees An optional numeric value indicating the number of trees in
#' this diameter-height class. Defaults to 1 if \code{NULL}.
#' @param quiet A logical value. If \code{TRUE}, suppresses any informational messages.
#'
#' @return A numeric vector
#'
#' @export
#'
#' @details
#' The function estimates biomass using validated allometric models available in the
#' dataset [biomass_models]. The available models include:
#'
#' - **[eq_biomass_ruiz_peinado_2011()]**: Developed for softwood species in Spain.
#' - **[eq_biomass_ruiz_peinado_2012()]**: Developed for hardwood species in Spain.
#'
#' Users can check the list of supported species and their corresponding components
#' in [biomass_models].
#'
#' If you would like to suggest additional models, please open a new issue on GitHub.
#' 
#' @seealso [eq_biomass_ruiz_peinado_2011()], [eq_biomass_ruiz_peinado_2012()], [biomass_models]
#'
#' @examples
#' # Calculate biomass for a single tree
#' silv_predict_biomass(
#'   diameter = 45,
#'   height   = 22,
#'   model    = eq_biomass_ruiz_peinado_2011("Pinus pinaster")
#' )
silv_predict_biomass <- function(
    diameter = NULL,
    height   = NULL,
    model,
    ntrees = NULL,
    quiet  = FALSE) {

  # 0. Handle errors and setup
  ## 0.2. Ensure species has same length as the rest, and ntrees = 1 when ntrees = NULL
  if (is.null(ntrees)) ntrees <- rep(1, length(diameter))
  # species <- rep_len(species, length(diameter))

  # 1. Define a helper function to calculate biomass for a single tree
  if (model@equation %in% c("ruiz-peinado-2011", "ruiz-peinado-2012")) {

    ## function to calculate biomass
    calc_biomass <- function(d, h, n, sp) {
      ## select expression based on species
      selected_expr <- model@expression[model@expression$species == sp, ]$expression
      ##
      if (grepl("h", selected_expr)) {
        f1 <- function(d, h) eval(parse(text = selected_expr))
        biomass <- f1(d, h) * n
      } else {
        f2 <- function(d) eval(parse(text = selected_expr))
        biomass <- f2(d) * n
      }

      ## create a table with the outputs
      biomass_tbl <- data.frame(
        biomass  = ifelse(model@params$return_rmse, model@params$rmse, biomass),
        citation = model@doi,
        obs      = model@obs
      )

      return(biomass_tbl)
    }

  }  

  # 2. Vectorize the function to handle multiple inputs
  biomass_mat <- mapply(calc_biomass, diameter, height, ntrees, model@species)

  biomass_df <- biomass_mat |>
    t() |>
    data.frame()

  biomass_df[] <- lapply(biomass_df, unlist)

  # 3. Feedback messages
  if (!quiet) {
    dois <- unique(biomass_df$citation)
    obss <- unique(biomass_df$obs)
    if (length(dois) == 1) {
      cli::cli_alert_warning("Cite this model using {.url {dois}}")
      cli::cli_alert_info(obss)
    } else {
      cli::cli_alert_warning("Cite these models using <{paste0(dois, collapse = ', ')}>")
      cli::cli_alert_info("{paste0(obss, collapse = ', ')}")
    }
  }

  return(biomass_df$biomass)
}





#' Biomass equations for Spanish softwood species
#'
#' Allometric equations adjusted for Spanish softwood species
#'
#' @param species A character string specifying the scientific name of the tree
#' species. It can be a column name if all the species are included in this model.
#' See Details for available species.
#' @param component A character string specifying the tree component for biomass
#' calculation (e.g., "tree", "stem", "branches"). See Details.
#' @param return_rmse A logical value. If TRUE, the function returns the root
#' mean squared error (RMSE) of the selected model instead of the biomass value.
#'
#' @return A S7 list of parameters
#'
#' @export
#' 
#' @details
#' Users can check the list of supported species and their corresponding components
#' in [biomass_models].
#' 
#' @seealso [silv_predict_biomass()], [eq_biomass_ruiz_peinado_2012()], [biomass_models]
#'
#' @examples
#' ## get model parameters for silv_predict_biomass
#' eq_biomass_ruiz_peinado_2011("Pinus pinaster")
eq_biomass_ruiz_peinado_2011 <- function(species, component = "stem", return_rmse = FALSE) {

  # 0. Handle errors 
  if (component == "tree" & return_rmse) cli::cli_abort("RMSE is only available by single components.")
  
  # 1. Select equation
  ## 1.1. Filter model
  sel_model <- biomass_models[biomass_models$article_id == "ruiz-peinado-2011", ]
  ## 1.2. Filter tree species
  sel_species <- sel_model[sel_model$species %in% species, ]
  ## 1.3. Filter component
  sel_component <- sel_species[sel_species$component %in% component, ]
  ## 1.4. Check if there's a matching model
  if (nrow(sel_component) == 0) cli::cli_abort(
    "The combination of species-component-model doesn't match any available option.
    Check {.url https://cidree.github.io/silviculture/reference/biomass_models.html} for available models."
  )

  # 2. Return
  ModelBiomass(
    equation   = "ruiz-peinado-2011",
    species    = species,
    expression = data.frame(
      expression = sel_component$expression,
      species    = sel_component$species
    ),
    doi        = unique(sel_component$doi),
    obs        = unique(sel_component$obs),
    params     = list(
      return_rmse = return_rmse,
      rmse        = sel_component$rmse
    )
  )

}





#' Biomass equations for Spanish hardwood species
#'
#' Allometric equations adjusted for Spanish hardwood species
#'
#' @param species A character string specifying the scientific name of the tree
#' species. It can be a column name if all the species are included in this model.
#' See Details for available species.
#' @param component A character string specifying the tree component for biomass
#' calculation (e.g., "tree", "stem", "branches"). See Details.
#' @param return_rmse A logical value. If TRUE, the function returns the root
#' mean squared error (RMSE) of the selected model instead of the biomass value.
#'
#' @return A S7 list of parameters
#'
#' @export
#' 
#' @details
#' Users can check the list of supported species and their corresponding components
#' in [biomass_models].
#' 
#' @seealso [silv_predict_biomass()], [eq_biomass_ruiz_peinado_2011()], [biomass_models]
#'
#' @examples
#' ## get model parameters for silv_predict_biomass
#' eq_biomass_ruiz_peinado_2012("Quercus suber")
eq_biomass_ruiz_peinado_2012 <- function(species, component = "stem", return_rmse = FALSE) {

  # 0. Handle errors 
  if (component == "tree" & return_rmse) cli::cli_abort("RMSE is only available by single components.")
  
  # 1. Select equation
  ## 1.1. Filter model
  sel_model <- biomass_models[biomass_models$article_id == "ruiz-peinado-2012", ]
  ## 1.2. Filter tree species
  sel_species <- sel_model[sel_model$species %in% species, ]
  ## 1.3. Filter component
  sel_component <- sel_species[sel_species$component %in% component, ]
  ## 1.4. Check if there's a matching model
  if (nrow(sel_component) == 0) cli::cli_abort(
    "The combination of species-component-model doesn't match any available option.
    Check {.url https://cidree.github.io/silviculture/reference/biomass_models.html} for available models."
  )

  # 2. Return
  ModelBiomass(
    equation   = "ruiz-peinado-2012",
    species    = species,
    expression = data.frame(
      expression = sel_component$expression,
      species    = sel_component$species
    ),
    doi        = unique(sel_component$doi),
    obs        = unique(sel_component$obs),
    params     = list(
      return_rmse = return_rmse,
      rmse        = sel_component$rmse
    )
  )

}