

ModelHD <- S7::new_class(
  name    = "ModelHD",
  package = "silviculture",
  properties = list(
    equation = S7::new_property(S7::class_character, default = quote(list())),
    species  = S7::new_property(S7::class_character, default = quote(list())),
    doi      = S7::new_property(S7::class_character, default = quote(list())),
    params   = S7::new_property(S7::class_list, default = quote(list()))
  )
)





#' Estimates tree height from DBH
#'
#' This function estimates tree height using h-d equations. Currently, only equations
#' for Spain are available.
#'
#' @param diameter Numeric vector with diameters in cm
#' @param model A function. A function with the structure \code{eq_hd_*()} with
#' additional arguments depending on the model. Currently only [eq_hd_aitor2025()] 
#' is available.
#' @param quiet A logical value. If TRUE, suppresses any informational messages.
#'
#' @details
#' Details...#TODO
#' 
#' @references Reference #TODO
#' 
#' @seealso [eq_hd_aitor2025()]
#'
#' @return A numeric vector with predicted height
#' @export
#'
#' @examples
#' 1 + 1 #TODO
silv_predict_height <- function(diameter,
                                model,
                                quiet = FALSE) {
  
  # 0. Handle errors

  # 1. Apply selected model
  if (model@equation == "aitor2025") {
    ## 2. Aitor et al. 2025
    ## 2.1. Function to calculate the height rowwise (vectorized)
    calc_height <- function(d, sp) {      
      ## 2.2. Select a and b based on species
      selected_model <- model@params$species_params[model@params$species_params$species_name == sp, ]
      total_a <- selected_model$a
      total_b <- selected_model$b
      ## 2.4. Apply model
      1.3 + total_a * (log(1 + d))**total_b
    }

    # 2. Vectorize the function to handle multiple rows
    predicted_height <- mapply(calc_height, diameter, model@species)
    
  }

  ## 3. Return 
  if (!quiet) cli::cli_alert_warning("Cite this model using {.url {model@doi}}")
  return(predicted_height)

}





#' Estimates tree height from DBH
#'
#' This function is made to be used in [silv_predict_height()]. It implements the h-d equations
#' developed in Aitor et al. (2025). These equations had been adjusted using the Spanish
#' National Forest Inventory, and therefore, they should be used only within Spain. A wide
#' range of tree species are available (90).
#'
#' @param species A character string specifying the scientific name of the tree
#' species. It can be a column name if all the species are included in this model.
#' See Details for available species.
#' @param bioregion The biogreopgrahic region of the species. Available options are:
#' \code{mediterranean}, \code{atlantic}, \code{alpine}, and \code{macaronesian}
#' @param origin The origin of the stand. Available options are: \code{natural} and
#' \code{plantation}
#' @param mixture The mixture of the species. Available options are: \code{pure} and
#' \code{mix}
#'
#' @details
#' Details...#TODO
#' 
#' @references Reference #TODO
#' 
#' @seealso [silv_predict_height()]
#'
#' @return A numeric vector with predicted height
#' @export
#'
#' @examples
#' 1 + 1 #TODO
eq_hd_aitor2025 <- function(species, 
                            bioregion = "mediterranean", 
                            origin    = "natural", 
                            mixture   = "pure") {

  # 1. Build parameters
  ## 1.1. Species
  selected_params <- h_d_aitor_tbl[tolower(h_d_aitor_tbl$species_name) %in% tolower(species), ]
  ## 1.2. Mange non-available species
  na_species <- species[!species %in% selected_params$species_name]
  if (length(na_species) > 0) {
    cli::cli_warn("The species {na_species} is not available. Using a generic model.")
    ## add generic model to the table
    selected_params <- rbind(
      selected_params, 
      h_d_aitor_tbl[h_d_aitor_tbl$species_name == "All the species", ]
    )
    ## replace non-available species for "All the species"
    species[!species %in% selected_params$species_name] <- "All the species"
  }
  ## 1.3. Bioregion
  bioregion_a <- switch(
    bioregion,
    "mediterranean" = 0,
    "atlantic"      = 0.2015,
    "alpine"        = -0.0324,
    "macaronesian"  = 0.1838,
    cli::cli_abort("Invalid bioregion. It must be <mediterranean>, <atlantic>, <alpine>, or <macaronesian>.")
  )
  bioregion_b <- switch(
    bioregion,
    "mediterranean" = 0,
    "atlantic"      = -0.0691,
    "alpine"        = -0.0348,
    "macaronesian"  = -0.1995
  )
  ## 1.4. Origin
  origin_a <- switch(
    origin,
    "natural"    = 0,
    "plantation" = -0.0120,
    cli::cli_abort("Invalid origin. It must be <natural>, or <plantation>.")
  )
  origin_b <- switch(
    origin,
    "natural"    = 0,
    "plantation" = 0.0382
  )
  ## 1.5. Mixture
  mixture_a <- switch(
    mixture,
    "pure" = 0,
    "mix"  = 0.0180,
    cli::cli_abort("Invalid mixture It must be <pure>, or <mix>.")
  )
  mixture_b <- switch(
    mixture,
    "pure" = 0,
    "mix"  = -0.0390
  )
  ## 1.6. Calculate extra a and b
  extra_a <- mixture_a + origin_a + bioregion_a
  extra_b <- mixture_b + origin_b + bioregion_b
  ## 1.7. Sum extra a and b
  selected_params$a <- selected_params$a + extra_a
  selected_params$b <- selected_params$b + extra_a

  # 2. Return
  ModelHD(
    equation = "aitor2025",
    species  = species,
    doi      = "Placeholder", #TODO
    params   = list(
      species_params = selected_params
    )
  )

}

