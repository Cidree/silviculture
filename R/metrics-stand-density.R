

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
#'      )
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
#' The Stand Density Index (SDI) is relationship between the average tree size and 
#' density of trees per hectare.
#'
#' @template ntrees
#' @template dg
#' @param classify whether to classify the values using USDA thresholds
#' @param max_sdi used when \code{classify = TRUE}. The maximum SDi, which depends
#' on the species, stand type, and site
#' 
#' @return A numeric vector
#' @export
#' 
#' @details
#' The SDI has different interpretation depending on the species, location, and also
#' the management type (even-aged, uneven-aged...). The value of maximum SDI must
#' be determined from the literature and used carefully. The option \code{classify = TRUE}
#' will use this value to classify the SDI in low density (<24%), moderate density (24-35%),
#' high density (34-55%), and extremely high density (>55%).
#'
#' @examples
#' ## calculate SDI for a Pinus sulvestris stand (max 990)
#' silv_density_sdi(ntrees = 800, dg = 23.4, max_sdi = 990)
#' 
#' ## check base classification (other can be used)
#' silv_density_sdi(ntrees = 800, dg = 23.4, classify = TRUE, max_sdi = 990)
silv_density_sdi <- function(
  ntrees, 
  dg, 
  classify = FALSE, 
  max_sdi = NULL
) {

  # 0. Validate inputs
  assert_positive_numeric(ntrees, "ntrees")
  assert_positive_numeric(dg, "dg")
  assert_logical(classify, "classify")
  assert_same_length(ntrees, dg, names = c("ntrees", "dg"))


  # 1. Calculate SDI
  sdi <- ntrees * ((dg / 25.4)) ** 1.605

  # 2. Classify?
  if (classify) {

    ## assert inputs
    if (is.null(max_sdi)) cli::cli_abort("You must specify <max_sdi> when <classify = TRUE>")
    assert_numeric(max_sdi, "max_sdi")

    ## calculate
    sdi <- (sdi / max_sdi) * 100
    sdi <- dplyr::case_when(
      sdi <= 24            ~ "Low density",
      sdi > 24 & sdi <= 34 ~ "Moderate density",
      sdi > 34 & sdi <= 55 ~ "High density",
      sdi > 55             ~ "Extremely high density" 
    )
  } else if (!is.null(max_sdi)) {
    sdi <- (sdi / max_sdi) * 100
  }
  return(sdi)

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




