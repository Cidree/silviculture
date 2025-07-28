

EquationHD <- S7::new_class(
  name    = "EquationHD",
  package = "silviculture",
  properties = list(
    equation = S7::new_property(S7::class_character, default = quote(list())),
    params   = S7::new_property(S7::class_list, default = quote(list()))
  )
)



#' Estimates tree height from DBH
#'
#' This function estimates tree height using h-d equations.
#'
#' @param diameter Numeric vector with diameters in cm
#' @param species A character string specifying the scientific name of the tree
#' species.
#' @param equation A function. A function with the structure \code{eq_hd_*()} with
#' additional arguments depending on the equation. Currently only [eq_hd_aitor2025] 
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
                              species,
                              equation,
                              quiet = FALSE) {
  
  # 0. Handle errors
  ## 0.2. Ensure species has same length as the rest, when only 1 species and several
  ## diameters specified
  if (length(species) == 1) species <- rep_len(species, length(diameter))

  # 1. Apply equation
  ## 1.1. Get equation params
  params <- equation
  ## 1.2. Apply the correct equation
  if (params@equation == "aitor2025") {
    # 2. Aitor et al. 2015
    ## 2.1. Function to calculate the height rowwise (vectorized)
    calc_height <- function(d, sp) {
      ## 2.1. Get species params
      selected_params <- h_d_aitor_tbl[tolower(h_d_aitor_tbl$species_name) == tolower(sp), ]
      ## 2.2. If the species is not available...
      if (nrow(selected_params) == 0) {
        if (!quiet) cli::cli_warn("The species {sp} is not available. Using a generic equation.")
        selected_params <- h_d_aitor_tbl[h_d_aitor_tbl$species_name == "All the species", ]
      }
      ## 2.3. Calculate total a and b
      total_a <- selected_params$a + params@params$extra_a
      total_b <- selected_params$b + params@params$extra_b
      ## 2.4. Apply equation
      1.3 + total_a * (log(1 + d))**total_b
    }

    # 2. Vectorize the function to handle multiple rows
    predicted_height <- mapply(calc_height, diameter, species)
    
    ## 2.5. Return reference 
    if (!quiet) cli::cli_alert_warning("Cite this model using {.url #TODO}")
  }

  return(predicted_height)

}


#' Estimates tree height from DBH
#'
#' This function is made to be used in [silv_predict_height]. It implements the h-d equations
#' developed in Aitor et al. (2025). These equations had been adjusted using the Spanish
#' National Forest Inventory, and therefore, they should be used only within Spain. A wide
#' range of tree species are available (90).
#'
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
#' @return A numeric vector with predicted height
#' @export
#'
#' @examples
#' 1 + 1 #TODO
eq_hd_aitor2025 <- function(bioregion = "mediterranean", 
                            origin    = "natural", 
                            mixture   = "pure") {

  # 1. Build parameters
  ## 1.1. Bioregion
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
  ## 1.2. Origin
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
  ## 1.3. Mixture
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
  ## 1.4. Calculate extra a and b
  extra_a <- mixture_a + origin_a + bioregion_a
  extra_b <- mixture_b + origin_b + bioregion_b

  # 2. Return
  EquationHD(
    equation = "aitor2025",
    params   = list(
    extra_a = extra_a,
    extra_b = extra_b
    )
  )

}

