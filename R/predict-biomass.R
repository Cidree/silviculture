
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
#' @param diameter A numeric vector of tree diameters at breast height (in cm).
#' @param height A numeric vector of tree heights (in m).
#' @param model A function. A function with the structure \code{eq_biomass_*()} with
#' additional arguments depending on the model used.
#' @param ntrees An optional numeric value indicating the number of trees in
#' this diameter-height class. Defaults to 1 if \code{NULL}.
#' @param rcd An optional numeric vector of root collar diameters (in cm). Required
#' for \code{\link{eq_biomass_menendez_2022}}, which uses root collar diameter
#' instead of diameter at breast height. Defaults to \code{diameter} if \code{NULL}.
#' @param bp An optional numeric vector of biomass packing values (in m\ifelse{html}{\out{<sup>3</sup>}}{$^3$}). Required
#' for a subset of species in \code{\link{eq_biomass_menendez_2022}} (e.g.
#' \emph{Pinus halepensis}, \emph{Pinus nigra}, \emph{Quercus suber},
#' \emph{Evergreen broadleaves}).
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
#' - **[eq_biomass_montero_2005()]**: Developed for 35 Spanish species.
#' - **[eq_biomass_dieguez_aranda_2009()]**: Developed for 7 Galician species.
#' - **[eq_biomass_manrique_2017()]**: Developed for *Quercus petraea* and *Quercus pyrenaica*.
#' - **[eq_biomass_menendez_2022()]**: Developed for young plantations (< 30 years) of 18
#'   Spanish species. Uses `rcd` (root collar diameter) instead of `diameter`; some species
#'   require `bp` (biomass packing) instead of `rcd`.
#' - **[eq_biomass_cudjoe_2024()]**: Developed for *Pinus sylvestris* and *Quercus petraea*
#'   in Castille and León, Spain.
#'
#' Users can check the list of supported species and their corresponding components
#' in [biomass_models].
#'
#' If you would like to suggest additional models, please open a new issue on GitHub.
#' 
#' @seealso [biomass_models], [eq_biomass_montero_2005()], [eq_biomass_dieguez_aranda_2009()],
#' [eq_biomass_ruiz_peinado_2011()], [eq_biomass_ruiz_peinado_2012()], [eq_biomass_manrique_2017()], 
#' [eq_biomass_menendez_2022()], [eq_biomass_cudjoe_2024()] 
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
    rcd    = NULL,
    bp     = NULL,
    quiet  = FALSE) {

  # 0. Handle errors and setup
  ## 0.1. Default rcd to diameter if not provided
  if (is.null(rcd)) rcd <- diameter
  ## 0.2. Ensure ntrees = 1 when ntrees = NULL
  if (is.null(ntrees)) ntrees <- rep(1, length(diameter))

  # 1. Define a helper function to calculate biomass for a single tree
  calc_biomass <- function(d, h, n, sp, rcd_val, bp_val) {
    ## select expression based on species
    selected_rows <- model@expression$species == sp
    selected_expr <- model@expression[selected_rows, ]$expression
    selected_expr <- selected_expr[!is.na(selected_expr)]
    if (length(selected_expr) == 0) {
      cli::cli_abort(
        "The selected model contains no valid expression for species {.val {sp}}."
      )
    }
    ## if there are multiple expressions (AGB, BGB)
    selected_expr <- paste0(
      "(",
      paste(selected_expr, collapse = " + "),
      ")"
    )

    ## evaluate in a named environment that exposes all possible variable names
    eval_env <- list(d = d, h = h, rcd = rcd_val, bp = bp_val)
    biomass   <- eval(parse(text = selected_expr), envir = eval_env) * n

    metric <- biomass
    if (isTRUE(model@params$return_rmse)) {
      metric <- model@params$rmse[selected_rows]
      metric <- metric[!is.na(metric)]
      if (length(metric) != 1) {
        cli::cli_abort(
          "RMSE is only available when the selected species-component combination resolves to a single equation."
        )
      }
    } else if (isTRUE(model@params$return_r2)) {
      metric <- model@params$r2[selected_rows]
      metric <- metric[!is.na(metric)]
      if (length(metric) != 1) {
        cli::cli_abort(
          "R2 is only available when the selected species-component combination resolves to a single equation."
        )
      }
    }

    data.frame(
      biomass  = metric,
      citation = model@url,
      obs      = model@obs
    )
  }

  # 2. Vectorize the function to handle multiple inputs
  bp_vec  <- if (is.null(bp)) rep(NA_real_, length(diameter)) else bp
  biomass_mat <- mapply(calc_biomass, diameter, height, ntrees, model@species, rcd, bp_vec)

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
#' 
#' **Supported species (10):**
#' 
#' *Abies alba*, *Abies pinsapo*, *Juniperus thurifera*, *Pinus canariensis*,
#' *Pinus halepensis*, *Pinus nigra*, *Pinus pinaster*, *Pinus pinea*,
#' *Pinus sylvestris*, *Pinus uncinata*
#' 
#' **Available components:**
#' 
#' Aboveground / belowground groups (summed automatically):
#' 
#' * `"AGB"` — total aboveground biomass
#' * `"BGB"` — total belowground biomass (roots)
#' * `"tree"` — total tree biomass (AGB + BGB)
#' 
#' Tree structural groups:
#' 
#' * `"stem"` — stem wood
#' * `"branches"` — all branch fractions combined
#' * `"roots"` — roots (equivalent to BGB)
#' 
#' Individual tree components (species availability varies):
#' 
#' * `"thick branches"` — branches > 7 cm
#' * `"thick and medium branches"` — branches > 2 cm
#' * `"medium branches"` — branches 2–7 cm
#' * `"small branches and leaves"` — branches < 2 cm including leaves/needles
#' 
#' Users can check the full species–component matrix in [biomass_models].
#' 
#' @seealso [silv_predict_biomass()], [biomass_models], [eq_biomass_montero_2005()], [eq_biomass_dieguez_aranda_2009()],
#' [eq_biomass_ruiz_peinado_2012()], [eq_biomass_manrique_2017()], [eq_biomass_menendez_2022()], [eq_biomass_cudjoe_2024()] 
#'
#' @examples
#' ## Aboveground biomass for Pinus pinaster
#' eq_biomass_ruiz_peinado_2011("Pinus pinaster", "AGB")
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
#' 
#' **Supported species (13):**
#' 
#' *Alnus glutinosa*, *Castanea sativa*, *Ceratonia siliqua*, *Eucalyptus globulus*,
#' *Fagus sylvatica*, *Fraxinus angustifolia*, *Olea europaea*, *Populus x euramericana*,
#' *Quercus canariensis*, *Quercus faginea*, *Quercus ilex*, *Quercus pyrenaica*,
#' *Quercus suber*
#' 
#' **Available components:**
#' 
#' Aboveground / belowground groups (summed automatically):
#' 
#' * `"AGB"` — total aboveground biomass
#' * `"BGB"` — total belowground biomass (roots)
#' * `"tree"` — total tree biomass (AGB + BGB)
#' 
#' Tree structural groups:
#' 
#' * `"stem"` — stem wood
#' * `"branches"` — all branch fractions combined
#' * `"roots"` — roots (equivalent to BGB)
#' 
#' Individual tree components (species availability varies):
#' 
#' * `"stem and thick branches"` — stem together with branches > 7 cm
#' * `"thick branches"` — branches > 7 cm
#' * `"thick and medium branches"` — branches > 2 cm
#' * `"medium branches"` — branches 2–7 cm
#' * `"small branches"` — branches 0.5–2 cm
#' * `"small branches and leaves"` — branches < 2 cm including leaves
#' * `"medium branches, small branches and leaves"` — branches < 7 cm including leaves
#' 
#' Users can check the full species–component matrix in [biomass_models].
#' 
#' @seealso [silv_predict_biomass()], [biomass_models], [eq_biomass_montero_2005()], [eq_biomass_dieguez_aranda_2009()],
#' [eq_biomass_ruiz_peinado_2011()], [eq_biomass_manrique_2017()], [eq_biomass_menendez_2022()],
#' [eq_biomass_cudjoe_2024()] 
#'
#' @examples
#' ## Aboveground biomass for Quercus suber
#' eq_biomass_ruiz_peinado_2012("Quercus suber", "AGB")
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
    equation   = "ruiz-peinado-2012",
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
#' **Supported species (7):**
#' 
#' *Betula alba*, *Eucalyptus globulus*, *Eucalyptus nitens*, *Pinus pinaster*,
#' *Pinus radiata*, *Pinus sylvestris*, *Quercus robur*
#' 
#' **Available components:**
#' 
#' Aboveground / belowground groups (summed automatically):
#' 
#' * `"AGB"` — total aboveground biomass
#' * `"BGB"` — total belowground biomass (roots)
#' 
#' Tree structural groups:
#' 
#' * `"stem"` — stem fraction(s)
#' * `"branches"` — all branch fractions combined
#' * `"roots"` — roots (equivalent to BGB)
#' 
#' Individual tree components (species availability varies):
#' 
#' * `"stem and thick branches"` — stem together with thickest branches
#' * `"thick branches"` — branches > 7 cm
#' * `"medium branches"` — branches 2–7 cm
#' * `"small branches"` — branches 0.5–2 cm
#' * `"twigs"` — branches < 0.5 cm
#' * `"dry branches"` — dead attached branches
#' * `"leaves"` — foliage (including needles)
#' * `"roots"` — coarse roots
#' 
#' Users can check the full species–component matrix in [biomass_models].
#' 
#' @seealso [silv_predict_biomass()], [biomass_models], [eq_biomass_montero_2005()],
#' [eq_biomass_ruiz_peinado_2011()], [eq_biomass_ruiz_peinado_2012()], [eq_biomass_manrique_2017()], 
#' [eq_biomass_menendez_2022()], [eq_biomass_cudjoe_2024()] 
#'
#' @examples
#' ## Aboveground biomass for Pinus pinaster
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
      return_rmse = return_rmse,
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
#' **Supported species (35):**
#' 
#' *Abies alba*, *Abies pinsapo*, *Alnus glutinosa*, *Betula* spp., *Castanea sativa*,
#' *Ceratonia siliqua*, *Erica arborea*, *Eucalyptus* spp., *Fagus sylvatica*,
#' *Fraxinus* spp., *Ilex canariensis*, *Juniperus oxycedrus*, *Juniperus phoenicea*,
#' *Juniperus thurifera*, *Laurus azorica*, *Myrica faya*, *Olea europaea* var. *sylvestris*,
#' *Other broadleaves*, *Other conifers*, *Other laurel species*, *Pinus canariensis*,
#' *Pinus halepensis*, *Pinus nigra*, *Pinus pinaster*, *Pinus pinea*, *Pinus radiata*,
#' *Pinus sylvestris*, *Pinus uncinata*, *Populus x euramericana*, *Quercus canariensis*,
#' *Quercus faginea*, *Quercus ilex*, *Quercus pyrenaica*, *Quercus robur*, *Quercus suber*
#' 
#' **Available components:**
#' 
#' Aboveground / belowground groups (summed automatically):
#' 
#' * `"AGB"` — total aboveground biomass
#' * `"BGB"` — total belowground biomass (roots)
#' * `"all"` or `"tree"` — total tree biomass (AGB + BGB)
#' 
#' Tree structural groups:
#' 
#' * `"stem"` — stem fraction(s)
#' * `"branches"` — all branch fractions combined
#' * `"roots"` — roots (equivalent to BGB)
#' 
#' Individual tree components (species availability varies):
#' 
#' * `"stem"` — stem wood
#' * `"stem and thick branches"` — stem together with branches > 7 cm
#' * `"thick branches"` — branches > 7 cm
#' * `"medium branches"` — branches 2–7 cm
#' * `"small branches"` — branches < 2 cm
#' * `"leaves"` — foliage (including needles)
#' * `"roots"` — coarse roots
#' 
#' Users can check the full species–component matrix in [biomass_models].
#' 
#' @seealso [silv_predict_biomass()], [biomass_models], [eq_biomass_dieguez_aranda_2009()],
#' [eq_biomass_ruiz_peinado_2011()], [eq_biomass_ruiz_peinado_2012()], [eq_biomass_manrique_2017()], 
#' [eq_biomass_menendez_2022()], [eq_biomass_cudjoe_2024()] 
#'
#' @examples
#' ## Aboveground biomass for Pinus pinaster
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
#' **Supported species (2):**
#' 
#' * *Quercus petraea*
#' * *Quercus pyrenaica*
#' 
#' **Available components:**
#' 
#' Aboveground group (summed automatically):
#' 
#' * `"AGB"` — total aboveground biomass (sum of all components below)
#' 
#' Individual tree components:
#' 
#' * `"stem and thick branches"` — stem together with branches > 7 cm
#' * `"medium branches"` — branches 2–7 cm
#' * `"small branches"` — branches < 2 cm
#' 
#' Users can check the full species–component matrix in [biomass_models].
#' 
#' @seealso [silv_predict_biomass()], [biomass_models], [eq_biomass_montero_2005()], [eq_biomass_dieguez_aranda_2009()],
#' [eq_biomass_ruiz_peinado_2011()], [eq_biomass_ruiz_peinado_2012()], [eq_biomass_menendez_2022()], 
#' [eq_biomass_cudjoe_2024()] 
#'
#' @examples
#' ## Aboveground biomass for Quercus petraea
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
      return_rmse = return_rmse,
      comp        = sel_component$tree_component,
      r2          = round(sel_component$r2, 3),
      rmse        = round(sel_component$rmse, 1)
    )
  )

}





#' Biomass equations for young Spanish plantations
#'
#' Allometric equations for young (<30) plantations of 18 Spanish species including 
#' broadleaf and conifer species. Only aboveground biomass.
#'
#' @param species A character string specifying the scientific name of the tree
#' species. It can be a column name if all the species are included in this model.
#' See Details for available species.
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
#' **Supported species (18):**
#' 
#' *Betula* sp., *Fagus sylvatica*, *Juniperus thurifera*, *Pinus halepensis*,
#' *Pinus nigra*, *Pinus pinaster*, *Pinus pinea*, *Pinus radiata*, *Pinus sylvestris*,
#' *Quercus faginea*, *Quercus ilex*, *Quercus petraea*, *Quercus pyrenaica*,
#' *Quercus robur*, *Quercus suber*
#' 
#' Generic equations are also available for functional groups:
#' 
#' * `"Conifers"` — generic equation for conifer species
#' * `"Deciduous broadleaves"` — generic equation for deciduous broadleaf species
#' * `"Evergreen broadleaves"` — generic equation for evergreen broadleaf species
#' 
#' **Available components:**
#' 
#' * `"AGB"` — aboveground biomass (only component available; no `component` argument needed)
#' 
#' **Important — non-standard input variables:**
#' 
#' Unlike other biomass models, these equations were fitted on **young plantations
#' (< 30 years)** and use different predictor variables:
#' 
#' * Most species use `rcd` = root collar diameter (cm), **not** diameter at breast
#'   height. Pass it via the `rcd` argument of [silv_predict_biomass()].
#' * Some species (*Pinus halepensis*, *Pinus nigra*, *Quercus suber*,
#'   *Evergreen broadleaves*) use `bp` = biomass packing (m\ifelse{html}{\out{<sup>3</sup>}}{$^3$}). Pass it via the
#'   `bp` argument of [silv_predict_biomass()].
#' * *Betula* sp. uses only `h` (total height).
#' 
#' Users can check the full species–component matrix in [biomass_models].
#' 
#' @seealso [silv_predict_biomass()], [biomass_models], [eq_biomass_montero_2005()], [eq_biomass_dieguez_aranda_2009()],
#' [eq_biomass_ruiz_peinado_2011()], [eq_biomass_ruiz_peinado_2012()], [eq_biomass_manrique_2017()], 
#' [eq_biomass_cudjoe_2024()] 
#'
#' @examples
#' ## AGB for Fagus sylvatica using root collar diameter
#' eq_biomass_menendez_2022("Fagus sylvatica")
eq_biomass_menendez_2022 <- function(species, return_r2 = FALSE, return_rmse = FALSE) {

  # 0. Handle errors 
  if (return_r2 & return_rmse) cli::cli_abort("Only one of `return_r2` and `return_rmse` can be TRUE")
  
  # 1. Select equation
  ## 1.1. Filter model
  sel_model <- biomass_models[biomass_models$article_id == "menendez-2022", ]
  ## 1.2. Filter tree species
  sel_species <- sel_model[sel_model$species %in% species, ]
  if (nrow(sel_species) == 0) cli::cli_abort("Species is not supported by this model")

  # 2. Return
  ModelBiomass(
    equation   = "menendez-2022",
    species    = species,
    component  = "AGB",
    expression = data.frame(
      expression = sel_species$expression,
      species    = sel_species$species
    ),
    url        = unique(sel_species$doi_url),
    obs        = unique(sel_species$obs),
    params     = list(
      return_r2   = return_r2,
      return_rmse = return_rmse,
      comp        = sel_species$tree_component,
      r2          = round(sel_species$r2, 3),
      rmse        = round(sel_species$rmse, 1)
    )
  )

}





#' Biomass equations for 2 species in Castille and León (Spain)
#'
#' Allometric equations adjusted for *Quercus petraea*, and *Pinus sylvestris*
#' in Castille and León (Spain)
#'
#' @param species A character string specifying the scientific name of the tree
#' species. It can be a column name if all the species are included in this model.
#' See Details for available species.
#' @param component A character string specifying the tree component for biomass
#' calculation (e.g., "stem", "branches"). See Details.
#' @param return_rmse A logical value. If TRUE, the function returns the root
#' mean squared error (RMSE) of the selected model instead of the biomass value.
#'
#' @return A S7 list of parameters
#'
#' @export
#' 
#' @details
#' 
#' **Supported species (3):**
#' 
#' * *Pinus sylvestris*
#' * *Quercus petraea*
#' * `"mixed"` — mixed stand of *Pinus sylvestris* × *Quercus petraea*
#' 
#' **Available components:**
#' 
#' Aboveground group (summed automatically):
#' 
#' * `"AGB"` — total aboveground biomass (sum of all components below)
#' 
#' Individual tree components (species availability varies):
#' 
#' * `"stem"` — stem wood (all species)
#' * `"thick branches"` — branches > 7 cm (all species)
#' * `"medium branches and small branches"` — branches < 7 cm (all species)
#' * `"leaves"` — foliage/needles (*Pinus sylvestris* only)
#' 
#' Users can check the full species–component matrix in [biomass_models].
#' 
#' @seealso [silv_predict_biomass()], [biomass_models], [eq_biomass_montero_2005()], [eq_biomass_dieguez_aranda_2009()],
#' [eq_biomass_ruiz_peinado_2011()], [eq_biomass_ruiz_peinado_2012()], [eq_biomass_manrique_2017()],
#' [eq_biomass_menendez_2022()]
#'
#' @examples
#' ## Aboveground biomass for a mixed stand
#' eq_biomass_cudjoe_2024("mixed", "AGB")
eq_biomass_cudjoe_2024 <- function(species, component = "AGB", return_rmse = FALSE) {

  # 0. Handle errors 
  ## rename mixed to Excel file name
  if (species == "mixed") species <- "Pinus sylvestris x Quercus petraea"
  
  # 1. Select equation
  ## 1.1. Filter model
  sel_model <- biomass_models[biomass_models$article_id == "cudjoe-2024", ]
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
    equation   = "cudjoe-2024",
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
      r2          = round(sel_component$r2, 3),
      rmse        = round(sel_component$rmse, 1)
    )
  )

}
