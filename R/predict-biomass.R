
ModelBiomass <- S7::new_class(
  name    = "ModelBiomass",
  package = "silviculture",
  properties = list(
    equation   = S7::new_property(S7::class_character, default = quote(list())),
    species    = S7::new_property(S7::class_character, default = quote(list())),
    component  = S7::new_property(S7::class_character, default = quote(list())),
    expression = S7::new_property(S7::class_data.frame, default = quote(list())),
    url        = S7::new_property(S7::class_character, default = quote(list())),
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
  if (model@equation %in% c(
    "ruiz-peinado-2011", 
    "ruiz-peinado-2012")
  ) {

    ## function to calculate biomass
    calc_biomass <- function(d, h, n, sp) {
      ## select expression based on species
      selected_expr <- model@expression[model@expression$species == sp, ]$expression
      ## if there are multiple expressions (AGB, BGB)
      selected_expr <- paste0(
        "(",
        paste(selected_expr, collapse = " + "),
        ")"
      )
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
        citation = model@url,
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
    urls <- unique(biomass_df$citation)
    obss <- unique(biomass_df$obs)
    if (length(urls) == 1) {
      cli::cli_alert_warning("Cite this model using {.url {urls}}")
      cli::cli_alert_info(obss)
    } else {
      cli::cli_alert_warning("Cite these models using <{paste0(urls, collapse = ', ')}>")
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
#' @seealso [silv_predict_biomass()], [eq_biomass_ruiz_peinado_2012()], [eq_biomass_dieguez_aranda_2009()], 
#' [biomass_models]
#'
#' @examples
#' ## get model parameters for silv_predict_biomass
#' eq_biomass_ruiz_peinado_2011("Pinus pinaster")
eq_biomass_ruiz_peinado_2011 <- function(species, component = "stem", return_rmse = FALSE) {

  # 0. Handle errors 
  if (component %in% c("tree", "AGB", "BGB") & return_rmse) 
    cli::cli_abort("RMSE is only available for single components.")
  
  # 1. Select equation
  ## 1.1. Filter model
  sel_model <- biomass_models[biomass_models$article_id == "ruiz-peinado-2011", ]
  ## 1.2. Filter tree species
  sel_species <- sel_model[sel_model$species %in% species, ]
  if (nrow(sel_species) == 0) cli::cli_abort("Species is not supported by this model")
  ## 1.3. Filter component
  ## First try big groups AGB or BGB
  ## Then try tree group (branches, stem...)
  ## Then try tree component (thick branches, thin branches...)
  if (component %in% c("AGB", "BGB")) {
    sel_component <- sel_species[sel_species$biomass_group %in% component, ]
  } else {
    sel_component <- sel_species[sel_species$tree_group %in% component, ]
    if (nrow(sel_component) == 0) sel_component <- sel_species[sel_species$tree_component %in% component, ]
  }  
  ## 1.4. Check if there's a matching model
  if (nrow(sel_component) == 0) cli::cli_abort(
    "The combination of species-component-model doesn't match any available option.
    Check {.url https://cidree.github.io/silviculture/reference/biomass_models.html} for available models."
  )

  # 2. Return
  ModelBiomass(
    equation   = "ruiz-peinado-2011",
    species    = species,
    component  = component,
    expression = data.frame(
      expression = sel_component$expression,
      species    = sel_component$species
    ),
    url        = unique(sel_component$doi_url),
    obs        = unique(sel_component$obs),
    params     = list(
      return_rmse = return_rmse,
      comp        = sel_component$tree_component,
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
#' @seealso [silv_predict_biomass()], [eq_biomass_ruiz_peinado_2011()], [eq_biomass_dieguez_aranda_2009()], 
#' [biomass_models]
#'
#' @examples
#' ## get model parameters for silv_predict_biomass
#' eq_biomass_ruiz_peinado_2012("Quercus suber")
eq_biomass_ruiz_peinado_2012 <- function(species, component = "stem", return_rmse = FALSE) {

  # 0. Handle errors 
  if (component %in% c("tree", "AGB", "BGB") & return_rmse) 
    cli::cli_abort("RMSE is only available for single components.")
  
  # 1. Select equation
  ## 1.1. Filter model
  sel_model <- biomass_models[biomass_models$article_id == "ruiz-peinado-2012", ]
  ## 1.2. Filter tree species
  sel_species <- sel_model[sel_model$species %in% species, ]
  if (nrow(sel_species) == 0) cli::cli_abort("Species is not supported by this model")
  ## 1.3. Filter component
  ## First try big groups AGB or BGB
  ## Then try tree group (branches, stem...)
  ## Then try tree component (thick branches, thin branches...)
  if (component %in% c("AGB", "BGB")) {
    sel_component <- sel_species[sel_species$biomass_group %in% component, ]
  } else {
    sel_component <- sel_species[sel_species$tree_group %in% component, ]
    if (nrow(sel_component) == 0) sel_component <- sel_species[sel_species$tree_component %in% component, ]
  }  
  ## 1.4. Check if there's a matching model
  if (nrow(sel_component) == 0) cli::cli_abort(
    "The combination of species-component-model doesn't match any available option.
    Check {.url https://cidree.github.io/silviculture/reference/biomass_models.html} for available models."
  )

  # 2. Return
  ModelBiomass(
    equation   = "ruiz-peinado-2011",
    species    = species,
    component  = component,
    expression = data.frame(
      expression = sel_component$expression,
      species    = sel_component$species
    ),
    url        = unique(sel_component$doi_url),
    obs        = unique(sel_component$obs),
    params     = list(
      return_rmse = return_rmse,
      comp        = sel_component$tree_component,
      rmse        = sel_component$rmse
    )
  )

}






#' Biomass equations for Galician species
#'
#' Allometric equations adjusted for Galician (Spain) species
#'
#' @param species A character string specifying the scientific name of the tree
#' species. It can be a column name if all the species are included in this model.
#' See Details for available species.
#' @param component A character string specifying the tree component for biomass
#' calculation (e.g., "tree", "stem", "branches"). See Details.
#' @param return_r2 A logical value. If TRUE, the function returns the root
#' mean squared error (RMSE) of the selected model instead of the biomass value.
#' @param return_rmse A logical value. If TRUE, the function returns the root
#' mean squared error (RMSE) of the selected model instead of the biomass value.
#'
#' @return A S7 list of parameters
#'
#' @export
#' 
#' @details
#' 
#' There are seven species included in this model: *Pinus pinaster, Pinaster radiata, Pinus*
#' *sylvestris, Eucalyptus globulus, Eucalyptus nitens, Quercus robur*, and *Betula alba*
#' 
#' The tree components are divided into groups, and any of them can be introduced in the
#' component argument:
#' 
#' * **AGB**: all aboveground biomass components
#' * **BGB**: all belowground biomass compoponents
#' * **tree**: total tree biomass includying AGB and BGB
#' 
#' Then we have the second group of components, which are related to tree groups:
#' 
#' * **stem**: includes the stem and bark
#' * **branches**: includes all branches
#' * **roots**: includes the roots (same as BGB)
#' 
#' Finally, we have the last level, which includes tree components (not all of them
#' are available for all species): stem, bark, thick branches (>7cm), medium branches (2-7cm), 
#' thin branches (0.5-2cm), twigs (<0.5cm), dry branches, leaves, roots. In some species,
#' there's "stem and thick branches", instead of two groups.
#' 
#' Users can check the list of supported species and their corresponding components
#' in [biomass_models].
#' 
#' @seealso [silv_predict_biomass()], [eq_biomass_ruiz_peinado_2011()], [eq_biomass_ruiz_peinado_2012()], 
#' [eq_biomass_manrique_2017()], [eq_biomass_montero_2005()], [biomass_models]
#'
#' @examples
#' ## get model parameters for silv_predict_biomass
#' eq_biomass_dieguez_aranda_2009("Pinus pinaster", "AGB")
eq_biomass_dieguez_aranda_2009 <- function(species, component = "stem", return_r2 = FALSE, return_rmse = FALSE) {

  # 0. Handle errors 
  if (return_r2 & return_rmse) cli::cli_abort("Only one of `return_r2` and `return_rmse` can be TRUE")
  if (component %in% c("tree", "AGB", "BGB") & return_rmse) 
    cli::cli_abort("RMSE is only available for single components.")
  
  # 1. Select equation
  ## 1.1. Filter model
  sel_model <- biomass_models[biomass_models$article_id == "dieguez-aranda-2009", ]
  ## 1.2. Filter tree species
  sel_species <- sel_model[sel_model$species %in% species, ]
  if (nrow(sel_species) == 0) cli::cli_abort("Species is not supported by this model")
  ## 1.3. Filter component
  ## First try big groups AGB or BGB
  ## Then try tree group (branches, stem...)
  ## Then try tree component (thick branches, thin branches...)
  if (component %in% c("AGB", "BGB")) {
    sel_component <- sel_species[sel_species$biomass_group %in% component, ]
  } else {
    sel_component <- sel_species[sel_species$tree_group %in% component, ]
    if (nrow(sel_component) == 0) sel_component <- sel_species[sel_species$tree_component %in% component, ]
  }  
  ## 1.4. Check if there's a matching model
  if (nrow(sel_component) == 0) cli::cli_abort(
    "The combination of species-component-model doesn't match any available option.
    Check {.url https://cidree.github.io/silviculture/reference/biomass_models.html} for available models."
  )

  # 2. Return
  ModelBiomass(
    equation   = "dieguez-aranda-2009",
    species    = species,
    component  = component,
    expression = data.frame(
      expression = sel_component$expression,
      species    = sel_component$species
    ),
    url        = unique(sel_component$doi_url),
    obs        = unique(sel_component$obs),
    params     = list(
      return_r2   = return_r2,
      return_rmse = return_r2,
      comp        = sel_component$tree_component,
      r2          = round(sel_component$r2, 3),
      rmse        = round(sel_component$rmse, 1)
    )
  )

}





#' Biomass equations for Spanish species
#'
#' Allometric equations adjusted for Spanish species
#'
#' @param species A character string specifying the scientific name of the tree
#' species. It can be a column name if all the species are included in this model.
#' See Details for available species.
#' @param component A character string specifying the tree component for biomass
#' calculation (e.g., "tree", "stem", "branches"). See Details.
#' @param return_r2 A logical value. If TRUE, the function returns the root
#' mean squared error (RMSE) of the selected model instead of the biomass value.
#'
#' @return A S7 list of parameters
#'
#' @export
#' 
#' @details
#' 
#' There are 35 species included in the model.
#' 
#' The tree components are divided into groups, and any of them can be introduced in the
#' component argument:
#' 
#' * **AGB**: all aboveground biomass components
#' * **BGB**: all belowground biomass compoponents
#' * **tree** or **all**: total tree biomass includying AGB and BGB
#' 
#' Then we have the second group of components, which are related to tree groups:
#' 
#' * **stem**: includes the stem and bark
#' * **branches**: includes all branches
#' * **roots**: includes the roots (same as BGB)
#' 
#' Finally, we have the last level, which includes tree components (not all of them
#' are available for all species): stem, thick branches (>7cm), medium branches (2-7cm), 
#' thin branches (0.5-2cm), needles, leaves, roots. In some species,
#' there's "stem and thick branches", instead of two groups.
#' 
#' Users can check the list of supported species and their corresponding components
#' in [biomass_models].
#' 
#' @seealso [silv_predict_biomass()], [eq_biomass_ruiz_peinado_2011()], [eq_biomass_ruiz_peinado_2012()], 
#' [eq_biomass_manrique_2017()], [eq_biomass_dieguez_aranda_2009()], [biomass_models]
#'
#' @examples
#' ## get model parameters for silv_predict_biomass
#' eq_biomass_montero_2005("Pinus pinaster", "AGB")
eq_biomass_montero_2005 <- function(species, component = "stem", return_r2 = FALSE) {

  # 0. Handle errors 
  if (component %in% c("tree", "all", "AGB", "BGB") & return_r2) 
    cli::cli_abort("R2 is only available for single components.")
  
  # 1. Select equation
  ## 1.1. Filter model
  sel_model <- biomass_models[biomass_models$article_id == "montero-2005", ]
  ## 1.2. Filter tree species
  sel_species <- sel_model[sel_model$species %in% species, ]
  if (nrow(sel_species) == 0) cli::cli_abort("Species is not supported by this model")
  ## 1.3. Filter component
  ## First try big groups AGB or BGB
  ## Then try tree group (branches, stem...)
  ## Then try tree component (thick branches, thin branches...)
  if (component %in% c("AGB", "BGB")) {
    sel_component <- sel_species[sel_species$biomass_group %in% component, ]
  } else {
    sel_component <- sel_species[sel_species$tree_group %in% component, ]
    if (nrow(sel_component) == 0) sel_component <- sel_species[sel_species$tree_component %in% component, ]
  }  
  ## 1.4. Check if there's a matching model
  if (nrow(sel_component) == 0) cli::cli_abort(
    "The combination of species-component-model doesn't match any available option.
    Check {.url https://cidree.github.io/silviculture/reference/biomass_models.html} for available models."
  )

  # 2. Return
  ModelBiomass(
    equation   = "montero-2005",
    species    = species,
    component  = component,
    expression = data.frame(
      expression = sel_component$expression,
      species    = sel_component$species
    ),
    url        = unique(sel_component$doi_url),
    obs        = unique(sel_component$obs),
    params     = list(
      return_r2 = return_r2,
      comp      = sel_component$tree_component,
      r2        = round(sel_component$r2, 3)
    )
  )

}






#' Biomass equations two Quercus species
#'
#' Allometric equations adjusted for *Quercus petraea* and *Quercus pyrenaica* 
#' in Palencia, Spain
#'
#' @param species A character string specifying the scientific name of the tree
#' species. It can be a column name if all the species are included in this model.
#' See Details for available species.
#' @param component A character string specifying the tree component for biomass
#' calculation (e.g., "stem", "branches"). See Details.
#' @param return_r2 A logical value. If TRUE, the function returns the root
#' mean squared error (RMSE) of the selected model instead of the biomass value.
#' @param return_rmse A logical value. If TRUE, the function returns the root
#' mean squared error (RMSE) of the selected model instead of the biomass value.
#'
#' @return A S7 list of parameters
#'
#' @export
#' 
#' @details
#' 
#' There are two species in this model: *Quercus petraea* and *Quercus pyrenaica* 
#' 
#' The tree components include:
#' 
#' * **stem**: includes steam and the thickest branches
#' * **medium branches**
#' * **thin branches**
#' * **AGB**: total biomass, results of summing the previous three components
#' 
#' @seealso [silv_predict_biomass()], [eq_biomass_ruiz_peinado_2011()], [eq_biomass_ruiz_peinado_2012()], 
#' [eq_biomass_montero_2005()], [eq_biomass_dieguez_aranda_2009()], [biomass_models]
#'
#' @examples
#' ## get model parameters for silv_predict_biomass
#' eq_biomass_manrique_2017("Quercus petraea", "AGB")
eq_biomass_manrique_2017 <- function(species, component = "AGB", return_r2 = FALSE, return_rmse = FALSE) {

  # 0. Handle errors 
  if (return_r2 & return_rmse) cli::cli_abort("Only one of `return_r2` and `return_rmse` can be TRUE")
  if (component %in% c("tree", "all", "AGB", "BGB") & return_r2) 
    cli::cli_abort("R2 is only available for single components.")
  
  # 1. Select equation
  ## 1.1. Filter model
  sel_model <- biomass_models[biomass_models$article_id == "manrique-2017", ]
  ## 1.2. Filter tree species
  sel_species <- sel_model[sel_model$species %in% species, ]
  if (nrow(sel_species) == 0) cli::cli_abort("Species is not supported by this model")
  ## 1.3. Filter component
  ## First try big groups AGB or BGB
  ## Then try tree group (branches, stem...)
  ## Then try tree component (thick branches, thin branches...)
  if (component %in% c("AGB", "BGB")) {
    sel_component <- sel_species[sel_species$biomass_group %in% component, ]
  } else {
    sel_component <- sel_species[sel_species$tree_group %in% component, ]
    if (nrow(sel_component) == 0) sel_component <- sel_species[sel_species$tree_component %in% component, ]
  }  
  ## 1.4. Check if there's a matching model
  if (nrow(sel_component) == 0) cli::cli_abort(
    "The combination of species-component-model doesn't match any available option.
    Check {.url https://cidree.github.io/silviculture/reference/biomass_models.html} for available models."
  )

  # 2. Return
  ModelBiomass(
    equation   = "manrique-2017",
    species    = species,
    component  = component,
    expression = data.frame(
      expression = sel_component$expression,
      species    = sel_component$species
    ),
    url        = unique(sel_component$doi_url),
    obs        = unique(sel_component$obs),
    params     = list(
      return_r2   = return_r2,
      return_rmse = return_r2,
      comp        = sel_component$tree_component,
      r2          = round(sel_component$r2, 3),
      rmse        = round(sel_component$rmse, 1)
    )
  )

}