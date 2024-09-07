

#' Calculates dominant height
#'
#' @param nmax Index of first diametric class with > 100 trees; if there are no
#'    100 trees, it is the index of the maximum
#' @param ntress Number of trees per hectare
#' @param height Height of the diametric class
#'
#' @return A numeric vector
#' @keywords internal
calc_dominant_height <- function(nmax, ntress, height) {

  # initialize n and empty list
  n <- 0
  l <- list()

  # loop to accumulate biggest trees up to 100
  for (i in 1:nmax[1]) {
    ## sum previous trees plus new trees
    n <- n + ntress[i]
    ## are we over 100 trees already?
    if (n > 100) {
      new_trees <- ntress[i] - n + 100
      ## add to list and exit loop
      l[[i]] <- c(new_trees, height[i])
      break
    } else {
      ## add to list
      l[[i]] <- c(ntress[i], height[i])
    }
  }

  # Extract the first elements from each element of the list
  first_elements <- sapply(l, function(x) x[1])

  # Calculate the weighted sum
  weighted_sum <- sum(sapply(l, function(x) x[1] * x[2]))

  # Calculate the weighted mean
  weighted_sum / sum(first_elements)

}





#' Calculates weighted mean
#'
#' @param var An object containing the values whose weighted median is to be computed
#' @param wt A numerical vector of weights the same length as x giving the weights to use for elements of x
#'
#' @return A length-one numeric vector
#' @keywords internal
weighted_median <- function(var, wt) {
  sorted_data <- dplyr::arrange(data.frame(var, wt), var)
  cumulative_weight <- cumsum(sorted_data$wt)
  total_weight <- sum(sorted_data$wt)

  # Find the index where cumulative weight crosses half the total weight
  median_idx <- which(cumulative_weight >= total_weight / 2)[1]

  # Return the var that corresponds to this index
  sorted_data$var[median_idx]
}




#' Calculates weighted standard deviation
#'
#' @param var An object containing the values whose weighted median is to be computed
#' @param wt A numerical vector of weights the same length as x giving the weights to use for elements of x
#'
#' @return A length-one numeric vector
#' @keywords internal
weighted_sd <- function(var, wt) {
  # Weighted mean
  w_mean <- sum(var * wt) / sum(wt)

  # Weighted variance
  w_variance <- sum(wt * (var - w_mean)^2) / sum(wt)

  # Standard deviation is the square root of variance
  sqrt(w_variance)
}






# calc_n_simple <- function(students_t, max_n, cv, max.error) {
#
#   ## calculate n
#   n <- (students_t**2 * cv**2) / ((max.error * 100)**2 + (students_t**2 * cv**2 / max_n))
#   ceiling(n)
# }
