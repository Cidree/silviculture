# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

silv_diametric_class <- function(x,
                                 dmin             = 7.5,
                                 dmax             = NULL,
                                 class.length     = 5,
                                 include.lowest   = TRUE,
                                 return.intervals = FALSE) {

  # 0. Setup and handle errors
  ## 0.1. Copy object
  diameters <- x

  # 1. Create intervals depending on user input
  ## - If dmax is NULL, use max diameter from data
  if (is.null(dmax)) {
    cuts_vec <- seq(dmin, max({{ x }}), class.length)
  } else {
    message(
      glue::glue("There are {length(diameters[diameters > dmax])} diameter values greater than `dmax = {dmax}`. They will be included in the greatest class.")
    )
    diameters[diameters > dmax] <- dmax
    cuts_vec <- c(seq(dmin, (dmax), class.length), Inf)
  }

  # 2. Create intervals either ( ] or [ )
  if (include.lowest) {
    intervals_vec <- cut(
      x              = diameters,
      breaks         = cuts_vec,
      right          = FALSE,
      include.lowest = TRUE
    )
  } else {
    intervals_vec <- cut(
      x      = diameters,
      breaks = cuts_vec
    )
  }

  # 3. Return intervals or class center?
  if (!return.intervals) {
    intervals_vec <- cuts_vec[as.numeric(intervals_vec)] + (class.length / 2)
  } else {
    ## Drop redundant levels
    intervals_vec <- droplevels(intervals_vec)
  }

  # 4. Return object
  return(intervals_vec)

}
