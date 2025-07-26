#' Classify diameters in classes
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Classifies the measured diameters into classes of a specified length
#'
#' @param diameter A numeric vector of diameters
#' @param dmin The minimum inventory diameter in centimeters
#' @param dmax The maximum inventory diameter in centimeters. Values that
#'    are greater than `dmax` are included in the greatest class
#' @param class_length The length of the class in centimeters
#' @param include_lowest Logical. If TRUE (the default), the intervals are
#'    `[dim1, dim2)`. If FALSE, the intervals are `(dim1, dim2]`
#' @param return_intervals If FALSE, it returns the intervals. Otherwise (the
#'    default), it returns the class center
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' library(dplyr)
#' inventory_samples |>
#'   mutate(dclass = silv_diametric_class(diameter))
silv_diametric_class <- function(diameter,
                                 dmin             = 7.5,
                                 dmax             = NULL,
                                 class_length     = 5,
                                 include_lowest   = TRUE,
                                 return_intervals = FALSE) {
  
  ## DEPRECATED ---------------
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "silv_diametric_class()",
    details = "Function `silv_diametric_class() is deprecated in favour of `silv_tree_dclass()`, and it will be removed in the next release."
  )

  # 0. Setup and handle errors
  ## 0.1. Handle data type
  if (!is.logical(return_intervals)) cli::cli_abort("The argument `return_intervals` must be TRUE or FALSE")
  if (!is.logical(include_lowest)) cli::cli_abort("The argument `include_lowest` must be TRUE or FALSE")
  if (!is.numeric(diameter)) cli::cli_abort("`diameter` must be a numeric vector")
  if (!is.numeric(dmin)) cli::cli_abort("`dmin` must be a numeric vector")
  if (!is.numeric(dmax) && !is.null(dmax)) cli::cli_abort("`dmax` must be a numeric vector or NULL")
  if (!is.numeric(class_length)) cli::cli_abort("`class_length` must be a numeric vector")
  ## 0.2. Invalid values
  if (any(diameter < 0, na.rm = TRUE)) cli::cli_warn("Any value in `diameter` is less than 0. Review your data.")
  if (dmin <= 0) cli::cli_abort("`dmin` must be greater than 0")
  if (dmax <= 0 && !is.null(dmax)) cli::cli_abort("`dmax` must be greater than 0")
  if (class_length <= 0) cli::cli_abort("`class_length` must be greater than 0")
  ## 0.3. dmax must be > than dmin
  if (dmin >= dmax && is.numeric(dmax)) cli::cli_abort("`dmax` has to be greater than `dmin`")

  # 1. Create intervals depending on user input
  ## - If dmax is NULL, use max diameter from data
  if (is.null(dmax)) {
    cuts_vec <- seq(dmin, max(diameter, na.rm = TRUE) + class_length, class_length)
  } else {
    cli::cli_alert_info(
      "There are {length(diameter[diameter > dmax])} diameter values greater than `dmax = {dmax}`. They will be included in the greatest class."
    )
    diameter[diameter > dmax] <- dmax
    cuts_vec <- c(seq(dmin, dmax, class_length), Inf)
  }

  # 2. Create intervals either ( ] or [ )
  if (include_lowest) {
    intervals_vec <- cut(
      x              = diameter,
      breaks         = cuts_vec,
      right          = FALSE,
      include.lowest = TRUE,
      dig.lab        = 10
    )
  } else {
    intervals_vec <- cut(
      x       = diameter,
      breaks  = cuts_vec,
      dig.lab = 10
    )
  }

  # 3. Return intervals or class center?
  if (!return_intervals) {
    intervals_vec <- cuts_vec[as.numeric(intervals_vec)] + (class_length / 2)
  } else {
    ## Drop redundant levels
    intervals_vec <- droplevels(intervals_vec)
  }

  # 4. Return object
  return(intervals_vec)

}





#' Calculates the dominant height
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Calculates the dominant height using the Assman equation or the Hart equation
#'
#' @param diameter Numeric vector with diameter classes
#' @param height Numeric vector with averaged heights by diameter class
#' @param ntrees Optional. Numeric vector with number of trees per hectare.
#' Use this argument when you have aggregated data by diametric classes (see details).
#' @param which The method to calculate the dominant height (see details)
#'
#' @details
#' The dominant height \eqn{H_0} is the mean height of dominant trees, which is
#' less affected than overall mean height by thinning or other treatments.
#'
#' - \bold{Assman}: calculates the \eqn{H_0} as the mean height of the 100 thickest
#' trees per hectare
#'
#' - \bold{Hart}: calculates the \eqn{H_0} as the mean height of the 100 tallest
#' trees per hectare
#' 
#' When \code{ntrees = NULL}, the function will assume that each diameter and height
#' belongs to only one tree. If you have data aggregated by hectare, you'll use the
#' number of trees per hectare in this argument.
#'
#' @references Assmann, E. (1970) The principles of forest yield study: Studies in the
#' organic production, structure, increment, and yield of forest stands. Pergamon Press, Oxford.
#'
#' @return A numeric vector
#' @export
#' @include utils-not-exported.R
#'
#' @examples
#' ## calculate h0 for inventory data grouped by plot_id and species
#' library(dplyr)
#' inventory_samples |>
#' mutate(dclass = silv_diametric_class(diameter)) |>
#'   summarise(
#'     height = mean(height, na.rm = TRUE),
#'     ntrees = n(),
#'     .by    = c(plot_id, species, dclass)
#'   ) |>
#'   mutate(
#'     ntrees_ha = silv_ntrees_ha(ntrees, plot_size = 10),
#'     h0        = silv_dominant_height(dclass, height, ntrees_ha),
#'     .by       = c(plot_id, species)
#'   )
silv_dominant_height <- function(diameter,
                                 height,
                                 ntrees = NULL,
                                 which = "assman") {
  
  ## DEPRECATED ---------------
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "silv_dominant_height()",
    details = "Function `silv_dominant_height() is deprecated in favour of `silv_stand_dominant_height()`, and it will be removed in the next release."
  )

  # 0. Handle errors and setup
  ## 0.1. Errors
  if (!tolower(which) %in% c("assman", "hart")) cli::cli_abort("`which` must be either <assman> or <hart>.")
  if (!is.numeric(diameter)) cli::cli_abort("`diameter` must be a numeric vector")
  if (!is.numeric(height)) cli::cli_abort("`height` must be a numeric vector")
  ## 0.2. Invalid values
  if (any(diameter <= 0, na.rm = TRUE)) cli::cli_warn("Any value in `diameter` is less than 0. Review your data.")
  if (any(height <= 0, na.rm = TRUE)) cli::cli_warn("Any value in `height` is less than 0. Review your data.")

  # 1. Create a data frame with input variables
  if (is.null(ntrees)) {
    data <- data.frame(
      d  = diameter,
      h  = height,
      nt = 1
    )
  } else {
    data <- data.frame(
      d  = diameter,
      h  = height,
      nt = ntrees
    )
  }


  # 2. Calculate dominant height
  if (tolower(which) == "assman") {
    d0 <- data |>
      ## sort descending by diameter class
      dplyr::arrange(dplyr::desc(d)) |>
      dplyr::mutate(
        .cumtrees = cumsum(nt),
        .nmax     = which(.cumtrees >= 100)[1],
        .nmax     = if (is.na(.nmax[1])) which.max(.cumtrees) else .nmax,
        .do       = calc_dominant_height(.nmax, nt, h)
      ) |>
      dplyr::pull(.do)
  } else {
    d0 <- data |>
      ## sort descending by height
      dplyr::arrange(dplyr::desc(h)) |>
      dplyr::mutate(
        .cumtrees = cumsum(nt),
        .nmax     = which(.cumtrees >= 100)[1],
        .nmax     = if (is.na(.nmax[1])) which.max(.cumtrees) else .nmax,
        .do       = calc_dominant_height(.nmax, nt, h)
      ) |>
      dplyr::pull(.do)
  }

  # 3. If it's not vectorized, retrieve just one value
  if (is.null(ntrees)) d0[1] else d0

}





#' Calculates Lorey's Height
#' 
#'#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Tree's mean height weighted by basal area
#'
#' @param height Numeric vector of heights
#' @param g Numeric vector of basal areas
#' @param ntrees Optional. Numeric vector of number of trees per hectare.
#' Use this argument when you have aggregated data by diametric classes (see details).
#'
#' @return A numeric vector
#' @export
#'
#' @details
#' The function calculates Lorey's mean height according to:
#'
#' \deqn{h_L = \frac{\sum n_i g_i h_i}{\sum n_i g_i}}
#'
#' When ntrees is not provided (i.e. \code{ntrees = NULL}) the formula is simply
#' the weighted mean of the provided heights and basal areas:
#'
#' \deqn{h_L = \frac{\sum g_i h_i}{\sum g_i}}
#'
#' @examples
#' ## Calculate Lorey's Height by plot and species
#' library(dplyr)
#' inventory_samples |>
#'   mutate(g = silv_basal_area(diameter)) |>
#'   summarise(
#'     lh  = silv_lorey_height(height, g),
#'     .by = c(plot_id, species)
#'   )
silv_lorey_height <- function(height, g, ntrees = NULL) {

  ## DEPRECATED ---------------
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "silv_lorey_height()",
    details = "Function `silv_lorey_height() is deprecated in favour of `silv_stand_lorey_height()`, and it will be removed in the next release."
  )

  # 0. Handle errors
  if (length(height) != length(g)) cli::cli_abort("`height` and `g` must have the same length")
  if (is.numeric(ntrees) && length(ntrees) != length(g)) cli::cli_abort("`ntrees` must have the same length as `height` and `g`, or be NULL")

  # 1. Calculate
  if (is.null(ntrees)) {
    weighted.mean(height, g)
  } else {
    weighted.mean(height, g * ntrees)
  }

}





#' Calculates the quadratic mean diameter (QMD)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @param diameter Numeric vector of diameters or diameter classes
#' @param ntrees Numeric vector with number of trees of the diameter class per
#' hectare. If `ntrees = NULL`, the function will assume that each diameter
#' corresponds to only one tree. Therefore, the QMD will be calculated
#' for each individual tree
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' ## calculate dg for inventory data grouped by plot_id and species
#' library(dplyr)
#' inventory_samples |>
#' mutate(dclass = silv_diametric_class(diameter)) |>
#'   summarise(
#'     height = mean(height, na.rm = TRUE),
#'     ntrees = n(),
#'     .by    = c(plot_id, species, dclass)
#'   ) |>
#'   mutate(
#'     ntrees_ha = silv_ntrees_ha(ntrees, plot_size = 10),
#'     h0        = silv_dominant_height(dclass, height, ntrees_ha),
#'     dg        = silv_sqrmean_diameter(dclass, ntrees_ha),
#'     .by       = c(plot_id, species)
#'   )
#'
#' ## calculate dg for a vector of diameters
#' silv_sqrmean_diameter(c(12.5, 23.5, 14, 16, 18.5))
silv_sqrmean_diameter <- function(diameter,
                                  ntrees = NULL) {
  
  ## DEPRECATED ---------------
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "silv_sqrmean_diameter()",
    details = "Function `silv_sqrmean_diameter() is deprecated in favour of `silv_stand_qmean_diameter()`, and it will be removed in the next release."
  )

  # 0. Handle errors and setup
  if (is.numeric(ntrees) && length(ntrees) != length(diameter)) cli::cli_abort("`ntrees` must have the same length as `diameter` or be NULL")
  if (!is.numeric(diameter)) cli::cli_abort("`diameter` must be a numeric vector")
  ## 0.2. Invalid values
  if (any(diameter <= 0, na.rm = TRUE)) cli::cli_warn("Any value in `diameter` is less than 0. Review your data.")

  # 1. Calculate squared mean diameter
  if (is.null(ntrees)) {
    sqrt(
      weighted.mean(
        x = diameter**2,
        w = rep(1, length(diameter))
      )
    )
  } else {
    sqrt(
      weighted.mean(
        x = diameter**2,
        w = ntrees
      )
    )
  }

}





#' Calculates Basal Area
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Calculates Basal Area in square meters.
#'
#' @param diameter Numeric vector of diameters or diameter classes
#' @param ntrees Numeric vector with number of trees of the diameter class per
#'    hectare. If `ntrees = NULL`, the function will assume that each diameter
#'    corresponds to only one tree. Therefore, basal area will be calculated
#'    for each individual tree
#' @param units The units of the diameter (one of `cm`, `mm`, or `m`)
#'
#' @return A numeric vector
#' @export
#'
#' @details
#' The function uses the next formula:
#'
#' \eqn{G = \frac{\pi}{40000} \cdot D^2}
#'
#' where G is the basal area in \eqn{m^2}, and D is the diameter in the `units`
#' specified in the function. It is recommended to use the squared mean diameter
#' calculated with [silv_sqrmean_diameter]
#'
#' @examples
#' ## calculate G for inventory data grouped by plot_id and species
#' library(dplyr)
#' inventory_samples |>
#' mutate(dclass = silv_diametric_class(diameter)) |>
#'   summarise(
#'     height = mean(height, na.rm = TRUE),
#'     ntrees = n(),
#'     .by    = c(plot_id, species, dclass)
#'   ) |>
#'   mutate(
#'     ntrees_ha = silv_ntrees_ha(ntrees, plot_size = 10),
#'     dg        = silv_sqrmean_diameter(dclass, ntrees_ha),
#'     g         = silv_basal_area(dclass, ntrees_ha),
#'     .by       = c(plot_id, species)
#'   )
#'
#' ## calculate individual basal area
#' silv_basal_area(c(23, 11, 43.5, 94))
silv_basal_area <- function(diameter,
                            ntrees = NULL,
                            units = "cm") {
  
  ## DEPRECATED ---------------
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "silv_basal_area()",
    details = "Function `silv_basal_area() is deprecated in favour of `silv_tree_basal_area()`, and it will be removed in the next release."
  )

  # 0. Handle errors and set-up
  if (is.numeric(ntrees) && length(ntrees) != length(diameter)) cli::cli_abort("`ntrees` must have the same length as `diameter` or be NULL")
  if (!is.numeric(diameter)) cli::cli_abort("`diameter` must be a numeric vector")
  ## 0.2. Invalid values
  if (any(diameter <= 0, na.rm = TRUE)) cli::cli_warn("Any value in `diameter` is less than 0. Review your data.")
  ## 0.3. If ntrees = NULL, only one tree assumed
  if (is.null(ntrees)) ntrees <- rep(1, length(diameter))

  # 1. Calculate basal area
  switch(units,
    "cm" = (pi / 4) * (diameter / 100)**2 * ntrees,
    "mm" = (pi / 4) * (diameter / 1000)**2 * ntrees,
    "m"  = (pi / 4) * diameter**2 * ntrees,
    cli::cli_abort("Invalid `units`. Use <cm>, <mm>, or <m>")
  )

}





#' Hart or Hart-Becking spacing index
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Calculates the Hart Index or the Hart-Becking Index for even-aged stands
#'
#' @param h0 Numeric vector with dominant height
#' @param ntrees Numeric vector with number of trees of the dominant height per
#'    hectare
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
#'     h0     = silv_dominant_height(diameter, height),
#'     ntrees = n(),
#'     .by    = plot_id
#'   ) |>
#'   ## calculate number of trees per hectare
#'   mutate(ntrees_ha = silv_ntrees_ha(ntrees, plot_size = 14.1)) |>
#'   mutate(spacing = silv_spacing_index(h0, ntrees_ha))
silv_spacing_index <- function(h0,
                               ntrees,
                               which = "hart") {
  
  ## DEPRECATED ---------------
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "silv_spacing_index()",
    details = "Function `silv_spacing_index() is deprecated in favour of `silv_density_hart()`, and it will be removed in the next release."
  )

  # 0. Errors
  if (!is.numeric(ntrees)) cli::cli_abort("ntrees` must be numeric")
  if (!is.numeric(h0)) cli::cli_abort("`h0` must be numeric")
  if (length(h0) != length(ntrees)) cli::cli_abort("`h0` and `ntrees` must have the same length")

  # 1. Calculate spacing index
  switch(which,
    "hart"         = 10000 / h0 / sqrt(ntrees),
    "hart-becking" = sqrt(20000 / (ntrees * sqrt(3))) / h0 * 100,
    cli::cli_abort("`which` must be either <hart> or <hart-becking>")
  )

}





#' Calculates number of trees per hectare
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Calculates number of trees per hectare for a given plot size and shape
#'
#' @param ntrees A numeric vector representing the number of trees in a sampling plot
#' @param plot_size A numeric vector of length one for circular radius in meters;
#'    or a numeric vector of length two for each side of a rectangular plot shape
#' @param plot_shape The shape of the sampling plot. Either `circular` or `rectangular`
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
#'     ntrees_ha = silv_ntrees_ha(n, plot_size = 10)
#'   )
#'
#' ## Rectangular plot of 10x15 meters
#' inventory_samples |>
#'   count(plot_id, species) |>
#'   mutate(
#'     ntrees_ha = silv_ntrees_ha(
#'       n,
#'       plot_size = c(10, 15),
#'       plot_shape = "rectangular"
#'      )
#'   )
silv_ntrees_ha <- function(ntrees,
                           plot_size,
                           plot_shape = "circular") {
  
  ## DEPRECATED ---------------
  lifecycle::deprecate_warn(
    when    = "0.2.0",
    what    = "silv_ntrees_ha()",
    details = "Function `silv_ntrees_ha() is deprecated in favour of `silv_density_ntrees_ha()`, and it will be removed in the next release."
  )

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





#' Calculate Tree Biomass
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
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
#' silv_biomass(
#'   diameter = 45,
#'   height   = 22,
#'   species  = "Pinus pinaster",
#'   model    = "ruiz-peinado-2011"
#' )
silv_biomass <- function(
    diameter    = NULL,
    height      = NULL,
    ntrees      = NULL,
    species     = NULL,
    component   = "stem",
    model       = "ruiz-peinado-2012",
    return_rmse = FALSE,
    quiet       = FALSE) {
  
  ## DEPRECATED ---------------
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "silv_biomass()",
    details = "Function `silv_biomass() is deprecated in favour of `silv_predict_biomass()`, and it will be removed in the next release."
  )

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





#' Calculate Tree Volume
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' This function calculates the volume of a tree or logs using different formulas:
#' Pressler, Huber, Smalian, and Newton. The appropriate diameter and height
#' parameters must be provided depending on the selected formula.
#'
#' @param diameter_base A numeric vector. The diameter at the base of the tree
#' (required for Pressler, Smalian, and Newton formulas).
#' @param diameter_top A numeric vector. The diameter at the top of the tree
#' (required for Smalian and Newton formulas).
#' @param diameter_center A numeric vector. The diameter at the center of the
#' tree (required for Huber and Newton formulas).
#' @param diameter A numeric vector. The diameter at breast height (used in
#' Pressler formula if provided instead of `diameter_base`).
#' @param height A numeric vector. The tree or log height (required for all formulas).
#' @param formula Character. The volume formula to use. Options: `"pressler"`,
#' `"huber"`, `"smalian"`, `"newton"`. Default is `"pressler"`.
#' @param ntrees A numeric vector with number of trees of the same dimensions.
#' Default is 1.
#'
#' @return A numeric value representing the tree volume.
#' @examples
#' silv_volume(diameter_base = 30, height = 20, formula = "pressler")
#' silv_volume(diameter_center = 25, height = 15, formula = "huber")
#' silv_volume(diameter_base = 30, diameter_top = 20, height = 20, formula = "smalian")
#'
#' @export
silv_volume <- function(diameter_base   = NULL,
                        diameter_top    = NULL,
                        diameter_center = NULL,
                        diameter        = NULL,
                        height          = NULL,
                        formula         = "pressler",
                        ntrees          = NULL) {
  
  ## DEPRECATED ---------------
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "silv_volume()",
    details = "Function `silv_volume() is deprecated in favour of `silv_tree_volume()`, and it will be removed in the next release."
  )

  if (is.null(ntrees)) ntrees <- 1

  if (formula == "pressler") {
    cli::cli_alert_warning("When using Pressler formula, the height is assumed to be Pressler directrix point (i.e., the height at which the diameter of the stem is half the diameter in the base of the tree).")
  }

  ## feedback about units in 0.2.0
  cli::cli_alert_info("Since v. 0.2.0 the diameter is assumed to be in centimeters.")

  ## Apply formula
  volume_vec <- switch(formula,
                       "pressler" = if (!is.null(diameter)) (pi / 4) * (diameter / 100)**2 * (2 / 3) * height * ntrees else (pi / 4) * (diameter_base / 100)**2 * (2 / 3) * height * ntrees,
                       "huber"   = (pi / 4) * (diameter_center / 100)**2 * height * ntrees,
                       "smalian" = (pi / 8) * ((diameter_base / 100)**2 + (diameter_top / 100)**2) * height * ntrees,
                       "newton"  = (pi / 24) * ((diameter_base / 100)**2 + (diameter_top / 100)**2 + 4 * (diameter_center / 100)**2) * height * ntrees
  )

  return(volume_vec)
}