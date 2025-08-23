

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
  weighted_sum <- sum(sapply(l, function(x) x[1] * x[2]), na.rm = TRUE)

  # Calculate the weighted mean
  weighted_sum / sum(first_elements, na.rm = TRUE)

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





#' Calculates number of plots using Student's T
#'
#' @param students_t Student's T value
#' @param max_n maximum number of plots in the area
#' @param cv coefficient of variation
#' @param max_error maximum allowed relative error
#'
#' @return A length-one numeric vector
#' @keywords internal
calc_n_simple <- function(students_t, max_n, cv, max_error) {

  ## calculate n
  n <- (students_t**2 * cv**2) / ((max_error * 100)**2 + (students_t**2 * cv**2 / max_n))
  ceiling(n)
}





#' Calculates number of plots for optimal allocation with constant cost
#'
#' @param students_t Student's T value
#' @param pj proportion of each stratum
#' @param sj variance of each stratum
#' @param max_n maximum number of plots in the area
#' @param max_error maximum allowed absolute error
#'
#' @return A length-one numeric vector
#' @keywords internal
calc_n_optimal <- function(students_t, pj, sj, max_n, max_error) {

  ## numerator Pj * Sj
  pjsj_num <- sum(pj * sqrt(sj))**2

  ## denominator Pj * Sj2
  pjsj_den <- sum(pj * sj)

  ## calculate n
  n <- (students_t**2 * pjsj_num) / (max_error**2 + (students_t**2 * pjsj_den / max_n) )
  ceiling(n)
}





#' Calculates number of plots for proportional allocation
#'
#' @param students_t Student's T value
#' @param pj proportion of each stratum
#' @param sj variance of each stratum
#' @param max_n maximum number of plots in the area
#' @param max_error maximum allowed absolute error
#'
#' @return A length-one numeric vector
#' @keywords internal
calc_n_prop <- function(students_t, pj, sj, max_n, max_error) {

  ## numerator Pj * Sj2
  pjsj_num <- sum(pj * sj)

  ## denominator Pj * Sj2
  pjsj_den <- sum(pj * sj)

  ## calculate n
  n <- (students_t**2 * pjsj_num) / (max_error**2 + (students_t**2 * pjsj_den / max_n) )
  ceiling(n)
}





#' Calculates number of plots for optimal allocation with variable cost
#'
#' @param students_t Student's T value
#' @param pj proportion of each stratum
#' @param sj variance of each stratum
#' @param cost cost of each stratum
#' @param max_n maximum number of plots in the area
#' @param max_error maximum allowed absolute error
#'
#' @return A length-one numeric vector
#' @keywords internal
calc_n_cost <- function(students_t, pj, sj, cost, max_n, max_error) {

  ## numerator Pj * Sj
  pjsj_num1 <- sum(pj * sqrt(cost) * sqrt(sj))
  pjsj_num2 <- sum(pj * sqrt(sj) / sqrt(cost))

  ## denominator Pj * Sj2
  pjsj_den <- sum(pj * sj)

  ## calculate n
  n <- (students_t**2 * pjsj_num1 * pjsj_num2) / (max_error**2 + (students_t**2 * pjsj_den / max_n) )
  ceiling(n)
}
