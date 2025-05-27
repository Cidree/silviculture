

#' LiDAR-derived Height Diversity Index (LHDI)
#'
#' LiDAR metric that calculates the LiDAR Height Diversity Index, which
#' can be used in \code{lidR} \code{*_metrics} functions
#'
#' @param z coordinate Z (height) of the point
#' @param interval height of the intervals to calculate the metric
#'
#' @returns numeric
#' @export
#'
#' @references Listopad, C. M. C. S., Masters, R. E., Drake, J., Weishampel, J.,
#' & Branquinho, C. (2015). Structural diversity indices based on airborne LiDAR
#' as ecological indicators for managing highly dynamic landscapes. Ecological
#' Indicators, 57, 268–279. \doi{10.1016/j.ecolind.2015.04.017}
#'
#' @examples
#' 1 + 1 ## TODO
lid_lhdi <- function(z, interval = 0.5) {

  ## LDHI = -Σ [(pi) × ln (pi)]
  ## - pi: proportion of returns within the ith range
  ## - Each range has a length of 0.5 meters, until the maximum height

  ## Max height and minimum height
  zmax <- max(z)
  zmin <- min(z)
  ## Round zmin down to the nearest multiple of 0.5
  zmin <- floor(zmin / interval) * interval
  ## nparts: number of full 0.5m layers
  zparts <- seq(zmin, zmax, interval)
  # Total number of points
  n <- length(z)
  ## Loop starter
  ldhi  <- 0
  ## Calculates LHDI for each 0.5 meters layer
  for (id in seq_along(zparts)) {
    ## Part that we have to review in this iteration
    zpart <- zparts[id]
    ## N points whose height fall between the range (0-0.5, 0.5-1...)
    npoints_i <- length(which(z > zpart - interval & z < zpart))
    ## When we have points
    if (npoints_i > 0) {
      ldhi <- (ldhi + ((npoints_i / n) * log(npoints_i / n)))
    }
  }

  ## Return the inverse
  return(-ldhi)

}



#' Calculate Forest Fraction Cover from LiDAR Data
#'
#' This function calculates the forest fraction cover (Fcov) from LiDAR data.
#' The Fcov in LiDAR is defined as the proportion of first returns above a specified
#' height threshold (default: 5 meters) relative to the total number of first returns.
#'
#' @param z A numeric vector representing the heights of LiDAR returns
#' @param rn An integer vector indicating the return number for each LiDAR point.
#' First returns are identified by a value of `1`
#' @param th a numeric vector of length one specifying the height threshold
#'
#' @return A numeric value representing the forest fraction cover,
#'   which is the proportion of first returns with heights greater than 5 meters.
#' @examples
#' # Example data
#' z <- c(2, 6, 10, 4, 15)
#' rn <- c(1, 1, 2, 1, 1)
#'
#' # Calculate forest fraction cover
#' lid_fcov(z, rn)
#' @export
lid_fcov <- function(z, rn, th = 5) {
  first_returns <- z[rn == 1L]
  sum(first_returns > th) / length(first_returns)
}

