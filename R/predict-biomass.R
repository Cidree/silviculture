
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
#' @return A numeric vector with predicted tree biomass (kg).
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
#' # 1. Vector-based calculation: predict stem/tree biomass for Pinus pinaster
#' model <- eq_biomass_ruiz_peinado_2011("Pinus pinaster")
#' predicted_biomass <- silv_predict_biomass(
#'   diameter = c(20, 25, 30),
#'   height   = c(15, 17, 18),
#'   model    = model
#' )
#' print(predicted_biomass)
#' 
#' # 2. Dataset-based tutorial: apply to a forest inventory data frame
#' inventory <- data.frame(
#'   tree_id  = 1:3,
#'   species  = c("Pinus pinaster", "Pinus pinaster", "Pinus pinaster"),
#'   dbh_cm   = c(18.5, 22.1, 29.4),
#'   height_m = c(14.0, 16.5, 19.0)
#' )
#' 
#' # Apply prediction and append a new column to the dataset
#' inventory$biomass_kg <- silv_predict_biomass(
#'   diameter = inventory$dbh_cm,
#'   height   = inventory$height_m,
#'   model    = model
#' )
#' print(inventory)
#' 
#' # 3. Young plantation example (Menendez 2022 model) using rcd and bp
#' # Menendez 2022 equations use root collar diameter (rcd) and/or biomass packing (bp)
#' model_menendez <- eq_biomass_menendez_2022("Pinus pinaster")
#' predicted_young_pinaster <- silv_predict_biomass(
#'   rcd      = c(5.2, 7.1, 9.4),   # Root collar diameter in cm
#'   height   = c(2.1, 3.2, 4.5),   # Height in m
#'   model    = model_menendez
#' )
#' print(predicted_young_pinaster)
#' 
#' # For Pinus halepensis, Menendez 2022 requires biomass packing (bp)
#' model_halepensis <- eq_biomass_menendez_2022("Pinus halepensis")
#' predicted_young_halepensis <- silv_predict_biomass(
#'   bp     = c(0.005, 0.012),     # Biomass packing in m3
#'   model  = model_halepensis
#' )
#' print(predicted_young_halepensis)
silv_predict_biomass <- function(
    diameter = NULL,
    height   = NULL,
    model,
    ntrees = NULL,
    rcd    = NULL,
    bp     = NULL,
    quiet  = FALSE) {

  # 0. Handle errors and setup
  ## 0.0. Determine vector length based on inputs
  n <- if (!is.null(diameter)) {
    length(diameter)
  } else if (!is.null(rcd)) {
    length(rcd)
  } else if (!is.null(bp)) {
    length(bp)
  } else {
    0
  }

  if (is.null(diameter)) diameter <- rep(NA_real_, n)
  ## 0.1. Default rcd to diameter if not provided
  if (is.null(rcd)) rcd <- diameter
  ## 0.2. Ensure ntrees = 1 when ntrees = NULL
  if (is.null(ntrees)) ntrees <- rep(1, n)
  ## 0.3. Default height to NA_real_ if NULL
  if (is.null(height)) height <- rep(NA_real_, n)

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
#' @return A ModelBiomass object containing the configured model parameters and expressions.
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
#' Note that *Abies pinsapo* does not have a separate BGB equation (requesting `"BGB"` or `"roots"` will fail, though `"tree"` still works via its pre-summed formula).
#' 
#' Users can check all available species and components in the [biomass_models] dataset provided by the library.
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
  if (nrow(sel_component) == 0) {
    if (component %in% c("BGB", "roots") && "Abies pinsapo" %in% species) {
      cli::cli_abort("Model {.val ruiz-peinado-2011} does not include BGB equations for {.val Abies pinsapo}.")
    }
    cli::cli_abort(
      "The combination of species-component-model doesn't match any available option.
      Check {.url https://cidree.github.io/silviculture/reference/biomass_models.html} for available models."
    )
  }

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
#' @return A ModelBiomass object containing the configured model parameters and expressions.
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
#' Note that *Eucalyptus globulus* does not have a separate BGB equation (requesting `"BGB"` or `"roots"` will fail, though `"tree"` still works via its pre-summed formula).
#' 
#' Users can check all available species and components in the [biomass_models] dataset provided by the library.
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
  if (nrow(sel_component) == 0) {
    if (component %in% c("BGB", "roots") && "Eucalyptus globulus" %in% species) {
      cli::cli_abort("Model {.val ruiz-peinado-2012} does not include BGB equations for {.val Eucalyptus globulus}.")
    }
    cli::cli_abort(
      "The combination of species-component-model doesn't match any available option.
      Check {.url https://cidree.github.io/silviculture/reference/biomass_models.html} for available models."
    )
  }

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
#' @return A ModelBiomass object containing the configured model parameters and expressions.
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
#' 
#' Note that total-tree equations (`"tree"` / `"all"`) are not available for this model.
#' Also, *Eucalyptus globulus*, *Eucalyptus nitens*, and *Pinus pinaster* lack BGB equations (requesting `"BGB"` or `"roots"` will fail).
#' 
#' Users can check all available species and components in the [biomass_models] dataset provided by the library.
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
  if (nrow(sel_component) == 0) {
    if (component %in% c("tree", "all")) {
      cli::cli_abort("Model {.val dieguez-aranda-2009} does not include total-tree ('tree' / 'all') equations.")
    }
    if (component %in% c("BGB", "roots") && species %in% c("Eucalyptus globulus", "Eucalyptus nitens", "Pinus pinaster")) {
      cli::cli_abort("Model {.val dieguez-aranda-2009} does not include BGB equations for {.val {species}}.")
    }
    cli::cli_abort(
      "The combination of species-component-model doesn't match any available option.
      Check {.url https://cidree.github.io/silviculture/reference/biomass_models.html} for available models."
    )
  }

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
#' @return A ModelBiomass object containing the configured model parameters and expressions.
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
#' 
#' Note that for this model, `"tree"` (or `"all"`) is an independent regression equation fitted to total-tree data. It was **not** derived by summing the AGB and BGB equations.
#' Consequently, there is a numerical discrepancy between the direct `"tree"` estimation and the sum of separate `"AGB"` and `"BGB"` estimations (e.g. for *Pinus sylvestris* at diameter = 20 cm and height = 10 m, the direct total is 89.1 kg, while AGB + BGB is 115.9 kg, a 24% difference).
#' 
#' Also, the following 6 species have no BGB/roots equations in this model: *Abies pinsapo*, *Erica arborea*, *Eucalyptus* spp., *Ilex canariensis*, *Laurus azorica*, *Myrica faya* (requesting `"BGB"` or `"roots"` will fail).
#' 
#' Users can check all available species and components in the [biomass_models] dataset provided by the library.
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
  if (nrow(sel_component) == 0 || all(is.na(sel_component$expression))) {
    if (component %in% c("BGB", "roots") && species %in% c("Abies pinsapo", "Erica arborea", "Eucalyptus spp.", "Ilex canariensis", "Laurus azorica", "Myrica faya")) {
      cli::cli_abort("Model {.val montero-2005} does not include BGB equations for {.val {species}}.")
    }
    if (nrow(sel_component) == 0) {
      cli::cli_abort(
        "The combination of species-component-model doesn't match any available option.
        Check {.url https://cidree.github.io/silviculture/reference/biomass_models.html} for available models."
      )
    } else {
      cli::cli_abort("Model {.val montero-2005} does not include valid equations for component {.val {component}} and species {.val {species}}.")
    }
  }

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
#' @return A ModelBiomass object containing the configured model parameters and expressions.
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
#' 
#' Note that no belowground biomass (BGB / roots) or total-tree equations are available in the source paper.
#' 
#' Users can check all available species and components in the [biomass_models] dataset provided by the library.
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
  if (nrow(sel_component) == 0) {
    if (component %in% c("BGB", "roots", "tree", "all")) {
      cli::cli_abort("Model {.val manrique-2017} does not include BGB / total-tree equations.")
    }
    cli::cli_abort(
      "The combination of species-component-model doesn't match any available option.
      Check {.url https://cidree.github.io/silviculture/reference/biomass_models.html} for available models."
    )
  }

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
#' @return A ModelBiomass object containing the configured model parameters and expressions.
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
#' 
#' Note that no belowground biomass (BGB / roots) or total-tree equations are available in the source paper for this model.
#' 
#' Users can check all available species and components in the [biomass_models] dataset provided by the library.
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
#' @return A ModelBiomass object containing the configured model parameters and expressions.
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
#' 
#' Note that no belowground biomass (BGB / roots) or total-tree equations are available in the source paper for this model.
#' 
#' Users can check all available species and components in the [biomass_models] dataset provided by the library.
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
  if (nrow(sel_component) == 0) {
    if (component %in% c("BGB", "roots", "tree", "all")) {
      cli::cli_abort("Model {.val cudjoe-2024} does not include BGB / total-tree equations.")
    }
    cli::cli_abort(
      "The combination of species-component-model doesn't match any available option.
      Check {.url https://cidree.github.io/silviculture/reference/biomass_models.html} for available models."
    )
  }

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




#' Automatically Predict Biomass Using the Best Available Model
#'
#' Evaluates vectors of tree species, diameters, and heights, and automatically
#' selects the best available allometric model based on a specified priority.
#' If tree height is not provided, is NA, or is 0, the function automatically
#' falls back to the \code{\link{eq_biomass_montero_2005}} model.
#'
#' @param species A character vector specifying the scientific names of the tree species.
#' @param diameter A numeric vector of tree diameters at breast height (in cm).
#' @param height An optional numeric vector of tree heights (in m). Defaults to \code{NULL}.
#' @param component A character string specifying the tree component for biomass
#'   calculation (e.g., "tree", "stem", "branches"). Defaults to \code{"tree"}.
#' @param ntrees An optional numeric vector indicating the number of trees in
#'   each class. Defaults to \code{NULL} (equivalent to 1 tree per entry).
#' @param rcd An optional numeric vector of root collar diameters (in cm). Required
#'   for young plantation equations (\code{\link{eq_biomass_menendez_2022}}).
#' @param bp An optional numeric vector of biomass packing values (in m\ifelse{html}{\out{<sup>3</sup>}}{$^3$}). Required
#'   for some species in young plantation equations.
#' @param priority A character vector specifying the priority order for model selection.
#'   Defaults to \code{c("ruiz-peinado-2011", "ruiz-peinado-2012", "montero-2005", "dieguez-aranda-2009", "manrique-2017", "menendez-2022", "cudjoe-2024")}.
#' @param quiet A logical value. If \code{TRUE}, suppresses any informational messages and warnings.
#'
#' @return A \code{data.frame} containing two columns:
#'   \itemize{
#'     \item \code{biomass}: Numeric vector of predicted biomass values (in kg).
#'     \item \code{biomass_model}: Character vector specifying the model ID used.
#'   }
#'
#' @export
#'
#' @examples
#' # 1. Vector-based calculation: automatic model selection for mixed species
#' species_vec <- c("Pinus pinaster", "Quercus petraea")
#' d_vec <- c(20, 25)
#' h_vec <- c(12, 14)
#' auto_results <- silv_predict_biomass_auto(species_vec, d_vec, h_vec)
#' print(auto_results)
#'
#' # 2. Vector-based calculation: fallback to Montero 2005 when height is missing
#' fallback_results <- silv_predict_biomass_auto(species_vec, d_vec, height = NULL)
#' print(fallback_results)
#' 
#' # 3. Dataset-based tutorial: apply to a mixed-species forest inventory data frame
#' inventory <- data.frame(
#'   tree_id  = 1:3,
#'   species  = c("Pinus pinaster", "Quercus petraea", "Pinus sylvestris"),
#'   dbh_cm   = c(22.5, 18.0, 31.2),
#'   height_m = c(15.0, 11.5, 18.0)
#' )
#' 
#' # Run auto-selection and bind the results directly
#' biomass_data <- silv_predict_biomass_auto(
#'   species  = inventory$species,
#'   diameter = inventory$dbh_cm,
#'   height   = inventory$height_m
#' )
#' 
#' inventory_with_biomass <- cbind(inventory, biomass_data)
#' print(inventory_with_biomass)
silv_predict_biomass_auto <- function(
    species,
    diameter,
    height   = NULL,
    component = "tree",
    ntrees   = NULL,
    rcd      = NULL,
    bp       = NULL,
    priority = c("ruiz-peinado-2011", "ruiz-peinado-2012", "montero-2005", "dieguez-aranda-2009", "manrique-2017", "menendez-2022", "cudjoe-2024"),
    quiet    = FALSE) {

  # 1. Sanity Checks & Length Validations
  n_trees <- length(species)
  if (length(diameter) != n_trees) {
    cli::cli_abort("{.arg species} and {.arg diameter} must have the same length.")
  }
  if (!is.null(height) && length(height) != n_trees) {
    cli::cli_abort("{.arg species} and {.arg height} must have the same length.")
  }

  # Expand scalars/defaults to vectors of length n_trees
  if (is.null(ntrees)) {
    ntrees <- rep(1, n_trees)
  } else if (length(ntrees) == 1) {
    ntrees <- rep(ntrees, n_trees)
  } else if (length(ntrees) != n_trees) {
    cli::cli_abort("{.arg ntrees} must be of length 1 or the same length as {.arg species}.")
  }

  if (is.null(rcd)) {
    rcd <- diameter
  } else if (length(rcd) == 1) {
    rcd <- rep(rcd, n_trees)
  } else if (length(rcd) != n_trees) {
    cli::cli_abort("{.arg rcd} must be of length 1 or the same length as {.arg species}.")
  }

  if (is.null(bp)) {
    bp <- rep(NA_real_, n_trees)
  } else if (length(bp) == 1) {
    bp <- rep(bp, n_trees)
  } else if (length(bp) != n_trees) {
    cli::cli_abort("{.arg bp} must be of length 1 or the same length as {.arg species}.")
  }

  # Determine height availability per tree
  if (is.null(height)) {
    has_height <- rep(FALSE, n_trees)
    height_vec <- rep(NA_real_, n_trees)
  } else {
    has_height <- !is.na(height) & height > 0
    height_vec <- height
  }

  # 2. Find Best Model for each unique combination of (species, has_height)
  unique_df <- unique(data.frame(
    species = species,
    has_height = has_height,
    stringsAsFactors = FALSE
  ))

  unique_df$model <- mapply(
    .find_best_model,
    unique_df$species,
    MoreArgs = list(component = component, priority = priority),
    unique_df$has_height,
    USE.NAMES = FALSE
  )

  # Match back to the full tree lists
  matched_models <- unique_df$model[match(
    paste(species, has_height, sep = "_"),
    paste(unique_df$species, unique_df$has_height, sep = "_")
  )]

  # 3. Warn about unmatched entries
  na_indices <- which(is.na(matched_models))
  if (length(na_indices) > 0 && !quiet) {
    unsupported <- unique(species[na_indices])
    cli::cli_warn(
      "No compatible model was found in priority for species: {.val {unsupported}} with component {.val {component}}."
    )
  }

  # 4. Predict Biomass grouping by (species, model)
  biomass_vec <- rep(NA_real_, n_trees)
  
  unique_groups <- unique(data.frame(
    species = species,
    model = matched_models,
    stringsAsFactors = FALSE
  ))
  unique_groups <- unique_groups[!is.na(unique_groups$model), ]

  for (row_idx in seq_len(nrow(unique_groups))) {
    sp <- unique_groups$species[row_idx]
    m_id <- unique_groups$model[row_idx]
    
    idx <- which(species == sp & matched_models == m_id)
    if (length(idx) == 0) next
    
    fn_name <- paste0("eq_biomass_", gsub("-", "_", m_id))
    fn <- get(fn_name, envir = asNamespace("silviculture"), mode = "function")
    model_obj <- fn(species = sp, component = component)
    
    group_biomass <- silv_predict_biomass(
      diameter = diameter[idx],
      height = height_vec[idx],
      model = model_obj,
      ntrees = ntrees[idx],
      rcd = rcd[idx],
      bp = bp[idx],
      quiet = TRUE
    )
    
    biomass_vec[idx] <- group_biomass
  }

  # 5. Citations / feedback
  if (!quiet) {
    used_models <- unique(matched_models[!is.na(matched_models)])
    if (length(used_models) > 0) {
      sel_models_info <- biomass_models[biomass_models$article_id %in% used_models, ]
      urls <- unique(sel_models_info$doi_url)
      obss <- unique(sel_models_info$obs)
      if (length(urls) == 1) {
        cli::cli_alert_warning("Cite this model using {.url {urls}}")
        cli::cli_alert_info(obss)
      } else {
        cli::cli_alert_warning("Cite these models using <{paste0(urls, collapse = ', ')}>")
        cli::cli_alert_info("{paste0(obss, collapse = ', ')}")
      }
    }
  }

  data.frame(
    biomass = biomass_vec,
    biomass_model = matched_models,
    stringsAsFactors = FALSE
  )
}




#' Predict All Individual Biomass Components for Trees
#'
#' Predicts all available individual biomass components (e.g., stem, bark, branches,
#' roots) for the given trees in a single call, returning a wide data frame.
#'
#' @param species A character vector specifying the scientific names of the tree species.
#' @param diameter A numeric vector of tree diameters at breast height (in cm).
#' @param height An optional numeric vector of tree heights (in m). Defaults to \code{NULL}.
#' @param model_fn A function or character string. The constructer function of the model
#'   (e.g., \code{eq_biomass_ruiz_peinado_2011}) or the model ID string (e.g., \code{"ruiz-peinado-2011"}).
#' @param ntrees An optional numeric vector indicating the number of trees in
#'   each class. Defaults to \code{NULL}.
#' @param rcd An optional numeric vector of root collar diameters (in cm).
#' @param bp An optional numeric vector of biomass packing values (in m\ifelse{html}{\out{<sup>3</sup>}}{$^3$}).
#' @param quiet A logical value. If \code{TRUE}, suppresses any informational messages.
#'
#' @return A \code{data.frame} with the columns \code{species}, \code{diameter} (cm),
#'   \code{height} (m, if provided), and one additional numeric column (in kg) for each
#'   individual biomass component available for the selected species and model.
#'
#' @export
#'
#' @examples
#' # 1. Vector-based calculation: predict all components for Pinus pinaster
#' comp_results <- silv_predict_biomass_components(
#'   species = c("Pinus pinaster", "Pinus pinaster"),
#'   diameter = c(20, 25),
#'   height = c(12, 15),
#'   model_fn = eq_biomass_ruiz_peinado_2011
#' )
#' print(comp_results)
#'
#' # 2. Dataset-based tutorial: apply to a forest inventory data frame
#' inventory <- data.frame(
#'   tree_id  = 1:3,
#'   species  = c("Pinus pinaster", "Pinus pinaster", "Pinus pinaster"),
#'   dbh_cm   = c(18.5, 22.1, 29.4),
#'   height_m = c(14.0, 16.5, 19.0)
#' )
#' 
#' # Predict components for the entire dataset
#' comp_df <- silv_predict_biomass_components(
#'   species  = inventory$species,
#'   diameter = inventory$dbh_cm,
#'   height   = inventory$height_m,
#'   model_fn = "ruiz-peinado-2011"
#' )
#' 
#' # Combine and display results (excluding repeated identifier columns)
#' inventory_with_components <- cbind(
#'   inventory, 
#'   comp_df[, -(1:3)]
#' )
#' print(inventory_with_components)
silv_predict_biomass_components <- function(
    species,
    diameter,
    height   = NULL,
    model_fn,
    ntrees   = NULL,
    rcd      = NULL,
    bp       = NULL,
    quiet    = TRUE) {

  # 1. Resolve model_fn
  if (is.character(model_fn)) {
    fn_name <- if (startsWith(model_fn, "eq_biomass_")) {
      model_fn
    } else {
      paste0("eq_biomass_", gsub("-", "_", model_fn))
    }
    model_fn <- get(fn_name, envir = asNamespace("silviculture"), mode = "function")
  }

  fn_name <- .get_function_name(model_fn)
  if (is.null(fn_name)) {
    cli::cli_abort("The provided function is not a recognized model function in silviculture.")
  }
  
  article_id <- gsub("^eq_biomass_", "", fn_name)
  article_id <- gsub("_", "-", article_id)

  # 2. Sanity Checks & Length Validations
  n_trees <- length(species)
  if (length(diameter) != n_trees) {
    cli::cli_abort("{.arg species} and {.arg diameter} must have the same length.")
  }
  if (!is.null(height) && length(height) != n_trees) {
    cli::cli_abort("{.arg species} and {.arg height} must have the same length.")
  }

  # Expand vectors
  if (is.null(ntrees)) {
    ntrees <- rep(1, n_trees)
  } else if (length(ntrees) == 1) {
    ntrees <- rep(ntrees, n_trees)
  } else if (length(ntrees) != n_trees) {
    cli::cli_abort("{.arg ntrees} must be of length 1 or the same length as {.arg species}.")
  }

  if (is.null(rcd)) {
    rcd <- diameter
  } else if (length(rcd) == 1) {
    rcd <- rep(rcd, n_trees)
  } else if (length(rcd) != n_trees) {
    cli::cli_abort("{.arg rcd} must be of length 1 or the same length as {.arg species}.")
  }

  if (is.null(bp)) {
    bp <- rep(NA_real_, n_trees)
  } else if (length(bp) == 1) {
    bp <- rep(bp, n_trees)
  } else if (length(bp) != n_trees) {
    cli::cli_abort("{.arg bp} must be of length 1 or the same length as {.arg species}.")
  }

  # 3. Find unique components across the species in the input
  unique_species <- unique(species)
  sel_model <- biomass_models[biomass_models$article_id == article_id, ]
  
  # Check if species are supported by the model
  unsupported_species <- setdiff(unique_species, sel_model$species)
  if (length(unsupported_species) > 0) {
    cli::cli_abort("Species {.val {unsupported_species}} {?is/are} not supported by model {.val {article_id}}.")
  }

  sel_species <- sel_model[sel_model$species %in% unique_species, ]

  components <- unique(sel_species$tree_component)
  non_totals <- components[!components %in% c("tree", "agb")]
  if (length(non_totals) > 0) {
    components <- non_totals
  }

  # 4. Construct output dataframe
  res_df <- data.frame(
    species = species,
    diameter = diameter,
    stringsAsFactors = FALSE
  )
  if (!is.null(height)) {
    res_df$height <- height
  }

  # 5. Predict each component
  for (comp in components) {
    comp_values <- rep(NA_real_, n_trees)
    
    for (sp in unique_species) {
      idx <- which(species == sp)
      if (length(idx) == 0) next
      
      # Check support
      has_comp <- any(sel_species$species == sp & sel_species$tree_component == comp)
      if (!has_comp) next
      
      args <- names(formals(model_fn))
      if ("component" %in% args) {
        model_obj <- model_fn(species = sp, component = comp)
      } else {
        model_obj <- model_fn(species = sp)
      }
      
      comp_values[idx] <- silv_predict_biomass(
        diameter = diameter[idx],
        height = if (!is.null(height)) height[idx] else NULL,
        model = model_obj,
        ntrees = ntrees[idx],
        rcd = rcd[idx],
        bp = bp[idx],
        quiet = TRUE
      )
    }
    
    # Capitalize acronyms for column names
    col_name <- comp
    if (col_name == "agb") col_name <- "AGB"
    if (col_name == "bgb") col_name <- "BGB"
    
    res_df[[col_name]] <- comp_values
  }

  # 6. Citations / feedback
  if (!quiet && nrow(sel_species) > 0) {
    urls <- unique(sel_species$doi_url)
    obss <- unique(sel_species$obs)
    if (length(urls) == 1) {
      cli::cli_alert_warning("Cite this model using {.url {urls}}")
      cli::cli_alert_info(obss)
    } else {
      cli::cli_alert_warning("Cite these models using <{paste0(urls, collapse = ', ')}>")
      cli::cli_alert_info("{paste0(obss, collapse = ', ')}")
    }
  }

  return(res_df)
}




# --- Internal Helpers ---

.find_best_model <- function(species, component, priority, has_height) {
  available_priorities <- if (has_height) priority else "montero-2005"
  
  for (model_id in available_priorities) {
    fn_name <- paste0("eq_biomass_", gsub("-", "_", model_id))
    if (!exists(fn_name, envir = asNamespace("silviculture"), mode = "function")) {
      next
    }
    fn <- get(fn_name, envir = asNamespace("silviculture"), mode = "function")
    
    args <- names(formals(fn))
    
    res <- tryCatch({
      if ("component" %in% args) {
        fn(species = species, component = component)
      } else {
        if (component == "AGB") {
          fn(species = species)
        } else {
          cli::cli_abort("Component not supported")
        }
      }
      TRUE
    }, error = function(e) {
      FALSE
    })
    
    if (res) {
      return(model_id)
    }
  }
  return(NA_character_)
}

.get_function_name <- function(fun) {
  ns <- asNamespace("silviculture")
  for (name in names(ns)) {
    if (is.function(ns[[name]]) && identical(ns[[name]], fun)) {
      return(name)
    }
  }
  return(NULL)
}
