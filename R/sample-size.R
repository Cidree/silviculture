


StratifiedSampleSize <- S7::new_class(
  name    = "SampleSize",
  package = "silviculture",
  properties = list(
    results        = S7::new_property(S7::class_data.frame, default = quote(data.frame())),
    strata_error   = S7::new_property(S7::class_data.frame, default = quote(data.frame())),
    sampling_error = S7::new_property(S7::class_data.frame, default = quote(data.frame())),
    sampling_opts  = S7::new_property(S7::class_list, default = quote(list()))
  )
)


SimpleSampleSize <- S7::new_class(
  name    = "SampleSize",
  package = "silviculture",
  properties = list(
    sampling_res  = S7::new_property(S7::class_list, default = quote(list())),
    sampling_opts = S7::new_property(S7::class_list, default = quote(list()))
  )
)





#' Calculates sample size for a stratified sampling
#' 
#' Calculates the sample size needed for a stratified inventory, estimated from 
#' pilot inventory data.
#'
#' @param data a `data.frame` of pilot inventory data
#' @param x name of the variable in `data` that was measured (e.g. basal area, volume)
#' @param strata name of the variable in `data` with the name of the stratum
#' @param total_area name of the variable in `data` with the area of the stratum
#' @param plot_size a numeric vector of length one with plot size in squared meters
#' @param method a charater vector of length one with the id of the method. Available
#' options are `optimal`, `cost`, and `prop`. See details
#' @param cost name of the variable in `data` with the average cost of measuring one plot
#' of the stratum. Used with `method = 'cost'` for sample size, and for message output in
#' other methods
#' @param max_error maximum allowed relative error
#' @param conf_level confidence level
#' @param max_iter maximum number of iteration to find the plot size
#' @param currency currency to be shown in console output when using `method = 'cost'`
#' @param quiet if \code{TRUE}, messages will be supressed
#' 
#' @importFrom stats var
#' @importFrom utils tail
#'
#' @returns 
#' S7 `StratifiedSampleSize` object with:
#' 
#' * **results**: `data.frame` with the main results by stratum
#' 
#' * **strata_error**: `data.frame` with maximum absolute error \eqn{\mp} C.I (max_abs_error, 
#' x_min, x_max), and the esimator of the typical error \eqn{\mp} C.I (sampling error, x_ci_lo, 
#' x_ci_hi)
#' 
#' * **sampling_error**: `data.frame` with the maximum absolute error \eqn{\mp} C.I (max_abs_error, 
#' x_min, x_max), and the typical sampling error of the weighted mean \eqn{\mp} C.I (sampling error, 
#' x_ci_lo, x_ci_hi)
#' 
#' * **sampling_opts**: `list` with function options
#' 
#' @export
#' 
#' @details
#' Stratified Sampling calculates the number of plots to be inventored in different strata.
#' For instance, you might have *Pinus sylvestris* and *Pinus pinaster* plots in the same
#' forest, and you might want to get the optimal number of plots for field inventory of each
#' stratum, for a given maximum relative error (e.g. 5%), and with a certain level of confidence 
#' (e.g 95%). Of course, the area of *P. sylvestris* will be different than the area occupied by
#' *P. pinaster*. For instance, the total area of *P. sylvestris* could be 100 ha, while the area
#' of *P. pinaster* could be 200 ha. Therefore, you need to create a pilot inventory and measure
#' a variable such as basal area maybe in 5 pilot plots of *P. sylvestris* and 7 pilot plots of
#' *P. pinaster*. With that data collected, you can use three stratified sample size methods:
#' 
#' - **Optimal Allocation with Constant Cost**: using `method = 'optimal'`. The sampling units
#' are distributed within the different strata taking into account the size (e.g. 100 ha vs 200 
#' ha) and the heterogeinity (e.g. differences in basal area). It minimizes the number of 
#' sampling units.
#' 
#' \eqn{n = \frac{t^2_{n - m} \cdot (\sum^{j = m}_{j = 1} P_j \cdot s_j)^2 }{\epsilon^2 + \frac{t^2_{n - m} \cdot \sum^{j = m}_{j = 1} P_j \cdot s_j^2}{N}}}
#' 
#' - **Optimal Allocation with Variable Cost**: using `method = 'cost'`. This method needs to know
#' the cost of a sampling unit in each strata. It will minimize the cost of the inventory,
#' taking into account the size, the heterogeinity, and the cost of the sampling unit of the strata.
#' 
#' \eqn{n = \frac{t^2_{n-m} \cdot (\sum^{j = m}_{j = 1} \cdot P_j \cdot s_j \cdot \sqrt{c_j}) \cdot (\sum^{j = m}_{j = 1} \cdot \frac{P_j \cdot s_j}{\sqrt{c_j}})}{\epsilon^2 + \frac{t^2_{n - m} \cdot \sum^{j = m}_{j = 1} P_j \cdot s_j^2}{N}}}
#' 
#' - **Proportional Allocation**: using `method = 'prop'`. The sampling units are distributed
#' proportional to the size of the strata. In the example, 33% of the estimated sampling
#' units will be allocated to *P. sylvestris* and 66% to *P. pinaster*.
#' 
#' \eqn{n = \frac{t^2_{n - m} \cdot \sum^{j = m}_{j = 1} P_j \cdot s_j^2 }{\epsilon^2 + \frac{t^2_{n - m} \cdot \sum^{j = m}_{j = 1} P_j \cdot s_j^2}{N}}}
#' 
#' Where:
#' 
#' - **n**: estimated sample size
#' 
#' - **t**: the value of student's t
#' 
#' - **\eqn{P_j}**: proportion of pilot plots of \eqn{j^{th}} strata
#' 
#' - **\eqn{s_j}**: standard deviation of `x`
#' 
#' - **\eqn{s_j^2}**: variance of `x`
#' 
#' - **N**: population size (number of plots of `plot_size` that fit in `total_area`)
#' 
#' - **\eqn{\epsilon}**: maximum allowed absolute error. Calculated from `x` and `max_error`
#' 
#' - **N**: the size of the pilot inventory
#' 
#' @importFrom stats qt sd
#'
#' @examples
#' ## read pilot inventory ficticious data
#' data_path <- system.file("extdata/pilot_inventory.csv", package = "silviculture")
#' inventory_tbl <- read.csv(data_path)
#' 
#' ## calculate sample size
#' sample_size_list <- silv_sample_size_stratified(
#'   data  = inventory_tbl,
#'   x     = basal_area,
#'   strata = stratum,
#'   total_area = area,
#'   method = "optimal",
#'   cost = cost,
#'   plot_size = 100,
#'   conf_level = .95,
#'   max_error = .05
#' )
silv_sample_size_stratified <- function(data,
                                        x, 
                                        strata,
                                        total_area,
                                        plot_size,
                                        method     = "optimal",
                                        cost       = NA,
                                        max_error  = 0.05,
                                        conf_level = 0.95,
                                        max_iter   = 1000,
                                        currency   = "EUR",
                                        quiet      = FALSE) {
  
  # 0. Handle errors
  method_name <- switch(method,
    "optimal" = "Optimal Allocation with Constant Cost",
    "cost"    = "Optimal Allocation with Variable Cost",
    "prop"    = "Proportional Allocation",
    cli::cli_abort("Invalid method name.")
  )


  # 1. Get initial params
  ## 1.1. Calculate mean, variance, and n
  data_summ <- data |> 
    dplyr::summarise(
      x_mean = mean({{ x }}),
      ## maximum allowed error
      max_abs_error = (x_mean * max_error) |> round(2),
      x_min  = (x_mean - x_mean * max_error) |> round(2), 
      x_max  = (x_mean + x_mean * max_error) |> round(2),  
      x_sd   = sd({{ x }}),
      x_var  = var({{ x }}),
      x_parc = dplyr::n(),
      cost   = mean({{ cost }}), # just the value
      .by    = c({{ strata }}, {{ total_area }})
    ) |> 
    ## proportion of each stratum
    dplyr::mutate(prop = {{ total_area }} / sum({{ total_area }})) |> 
    dplyr::arrange({{ strata }})
  ## store cost as a new variable (needed for message)
  cost <- data_summ$cost
  ## population size
  max_n <- sum(data_summ$area) / plot_size  
  ## add population size to each stratum
  data_summ <- cbind(data_summ, max_n = max_n * data_summ$prop)
  ## calculate Student's t for selected CI
  students_t <- qt((1 + conf_level) / 2, df = max_n)
  ## absolute error (weighted mean of the pilot inventory multiplied by maximum error)
  abs_error <- sum(data_summ$x_mean * data_summ$prop) * max_error
  ## calculate n (skip if it's too low)
  n <- switch(method,
    "optimal" =  calc_n_optimal(students_t, data_summ$prop, data_summ$x_var, max_n, abs_error),
    "prop" = calc_n_prop(students_t, data_summ$prop, data_summ$x_var, max_n, abs_error),
    "cost" = calc_n_cost(students_t, data_summ$prop, data_summ$x_var, data_summ$cost, max_n, abs_error)
  )
  ## if n is equal or less than strata, t-test cannot be computed
  if (n <= nrow(data_summ)) {
    cli::cli_warn("The estimated sample size {n}. Consider decreasing the sampling error")
    data_summ <- cbind(data_summ, n = n / nrow(data_summ))
    return(data_summ)
  }

  # 2. Iterate until convergence
  ## 2.1. initialize values
  final_n <- 0
  i       <- 1
  n_vals <- data.frame(i = numeric(), n = numeric())
  ## 2.2. Calculate final sample size (when n == final_n, or maximum iterations happens)
  while (n != final_n && i < max_iter) {
    ## calculate new student's t
    new_students_t <- qt((1 + conf_level) / 2, df = max(final_n - nrow(data_summ), nrow(data_summ)))
    ## update n
    n <- final_n
    ## calculate new n
    final_n <- switch(method,
      "optimal" =  calc_n_optimal(new_students_t, data_summ$prop, data_summ$x_var, max_n, abs_error),
      "prop"    = calc_n_prop(new_students_t, data_summ$prop, data_summ$x_var, max_n, abs_error),
      "cost"    = calc_n_cost(new_students_t, data_summ$prop, data_summ$x_var, data_summ$cost, max_n, abs_error),
      cli::cli_abort("Invalid method name.")
    )
    ## add to DF
    n_vals <- rbind(n_vals, data.frame(i = i, n = final_n))
    i <- i + 1
    ## skip before if there's a repeating pattern like [a, b, a, b, a, b]
    if (nrow(n_vals) >= 10) {
      last_vals <- tail(n_vals$n, 10)
      ## check if values are alternating in a pattern
      if (all(last_vals[c(TRUE, FALSE)] == last_vals[1]) &&
          all(last_vals[c(FALSE, TRUE)] == last_vals[2]) &&
          last_vals[1] != last_vals[2]) {
        break
      }
    }
  }
  ## 2.3. Return a warning if no convergence has been found after maximum iterations
  if (i == max_iter) {
    cli::cli_warn("No convergence found after {max_iter} iterations. Returning latest value.")
    final_n <- min(n, final_n)
  }
  ## 2.4 Calculate n by stratum and round up
  final_n <- switch(method,
    "optimal" = (data_summ$prop * sqrt(data_summ$x_var)) / (sqrt(sum(data_summ$prop * sqrt(data_summ$x_var))**2)) * final_n,
    "prop"    = final_n * data_summ$prop,
    "cost"    = (data_summ$prop * data_summ$x_sd / sqrt(data_summ$cost) / (sum(data_summ$prop * data_summ$x_sd / sqrt(data_summ$cost))) * final_n)
  ) |> ceiling()
  data_summ <- cbind(data_summ, n = final_n)
  ## 2.5. Calculate effort (n_parc per hectare)
  sampling_effort <- (final_n / data_summ$area * 10000) |> round(1)
  data_summ <- cbind(data_summ, sampling_effort = sampling_effort)
  # 3. Calculate errors 
  ## 3.1. Typical error - estimation of SD of the estimation error (by stratum)
  sd_x <- (data_summ$x_sd / sqrt(data_summ$n)) * sqrt(1 - (data_summ$n / data_summ$max_n))
  ## 3.2. Typical error of the sampling mean
  error_sampling_mean <- sqrt(sum(data_summ$prop**2 * sd_x**2))
  ## 3.3. Sampling error of the mean
  students_t <- qt((1 + conf_level) / 2, df = sum(data_summ$n))
  sampling_error <- students_t * error_sampling_mean
  ## 3.4. Create a table for stratum errors
  strata_error_df <- cbind(
    strata = dplyr::pull(data_summ, {{ strata }}),
    data_summ[, c("x_mean", "max_abs_error", "x_min", "x_max")],
    sampling_error = sd_x,
    x_ci_lo = data_summ$x_mean - sd_x,
    x_ci_hi = data_summ$x_mean + sd_x
  )
  ## 3.5. Create a table for global error
  x_mean <- sum(data_summ$prop * data_summ$x_mean)
  error_df <- data.frame(
    x_mean         = x_mean,
    max_abs_error  = x_mean * max_error,
    x_min          = x_mean - (x_mean * max_error),
    x_max          = x_mean + (x_mean * max_error),
    sampling_error = sampling_error,
    x_ci_lo        = x_mean - sampling_error,
    x_ci_hi        = x_mean + sampling_error
  )
  
  # 4. Display results
  if (!quiet) {
    cli::cli_h1("Pilot inventory")
    cli::cli_text("You are estimating the sample size using a Stratified Sampling using the {cli::col_br_yellow(method_name)} method.") 

    cli::cli_bullets(c(
      "*" = "Pilot inventory: {nrow(data)} plots",
      "*" = "Maximum allowed error: {max_error * 100}%",
      "*" = "Total sampling plots: {cli::col_yellow(cli::style_bold(sum(final_n)))}",
      "*" = "Actual error: {round(error_df$sampling_error / error_df$x_mean * 100, 2)}%"
    ))

    if (!all(is.na(cost))) {
        cli::cli_bullets(c(
          "*" = "Total cost of the inventory: {sum(data_summ$cost * data_summ$n)} {currency}"
        ))
    }

    cli::cli_h1("Operational inventory")
    for (i in 1:nrow(data_summ)) {
      # Extract current row data
      current_row <- data_summ[i, ]
      current_error <- strata_error_df[i, ]
      
      # Group name as h2
      cli::cli_h2("Stratum - {current_row |> dplyr::pull({{ strata }})}")
      
      # Pilot inventory
      cli::cli_bullets(c(
        "*" = "Pilot inventory with {current_row$x_parc} plots, in {(current_row |> dplyr::pull({{ total_area }}) / 10000)} ha"
      ))
      
      # Minimum sampling plots with error and CI (highlighted)
      cli::cli_bullets(c(
        "*" = "Minimum sampling plots: {cli::col_yellow(cli::style_bold(current_row$n))} with a relative error of {round(current_error$sampling_error / current_error$x_mean * 100, 2)}% ({conf_level * 100}% CI [{round(current_error$x_ci_lo, 2)}, {round(current_error$x_ci_hi, 2)}])"
      ))
      
      # Sampling effort
      cli::cli_bullets(c(
        "*" = "Sampling effort: {current_row$sampling_effort} plots/ha"
      ))

      if (!all(is.na(cost))) {
        cli::cli_bullets(c(
          "*" = "Cost per hectare: {current_row$cost * current_row$sampling_effort} {currency}/ha",
          "*" = "Total cost: {current_row$cost * current_row$n} {currency}"
        ))
      }
      
      # Add some spacing between groups
      cli::cli_text("")
    }
  }

  # 5. Metadata list
  metadata_lst <- list(
    method      = method,
    pilot_plots = max_n,
    max_error   = max_error,
    abs_error   = abs_error,
    conf_level  = conf_level
  )

  return(
    StratifiedSampleSize(
      results        = data_summ,
      strata_error   = strata_error_df,
      sampling_error = error_df,
      sampling_opts  = metadata_lst
    )
  )

}





#' Calculates sample size for a simple random sampling (SRS)
#' 
#' Calculates the sample size needed for a SRS inventory, estimated from 
#' pilot inventory data.
#'
#' @param x vector of the variable measured in the pilot inventory (e.g. basal area, volume)
#' @param plot_size a numeric vector of length one with plot size in squared meters
#' @param total_area total area of the study area in squared meters
#' @param max_error maximum allowed relative error
#' @param conf_level confidence level
#' @param max_iter maximum number of iteration to find the plot size
#' @param quiet if \code{TRUE}, messages will be supressed
#'
#' @returns SimpleSampleSize object
#' @export
#' 
#' @details
#' Sample size is very important to be optimized, since a small sample size
#' will entail a higher error, while a huge sample size will entail higher
#' costs. The SRS is typically used for random sampling, although it might
#' be used also for regular sampling. The number of samples is calculated using
#' the expression:
#' 
#' \eqn{n \ge \frac{t^2 \cdot CV^2}{\epsilon^2 + \frac{t^2 \cdot CV^2}{N}}}
#' 
#' Where:
#' - **t**: the value of student's t for given sample size of the pilot inventory
#' 
#' - **CV**: the coefficient of variation of `x`
#' 
#' - \eqn{\epsilon}: the relative error (`max_error`)
#' 
#' - **N**: the size of the pilot inventory
#' 
#' `x` is a variable measured in a pilot inventory. Let's say we measure forest
#' variables in 10 pilot plots, aiming at basal area measurement so we have to 
#' measure only the DBH. After some calculations, we will have the basal area
#' per hectare in each of the 10 plots. The sample size is then calculated from
#' the variation of these values and the error that we will allow.
#' 
#' @importFrom stats qt sd
#'
#' @examples
#' ## pilot inventory measuring 4 plots of 25x25 meters
#' ## total forest area 15 ha
#' ## measured variable (x): basal area per hectare
#' silv_sample_size_simple(
#'   x          = c(33, 37.5, 42, 35.2),
#'   plot_size  = 25 * 25,  # squared plot of 25x25
#'   total_area = 15 * 1e4, # 15 ha
#'   max_error  = 0.05,
#'   conf_level = 0.95,
#'   max_iter   = 100
#' )
silv_sample_size_simple <- function(x,
                                    plot_size,
                                    total_area,
                                    max_error  = 0.05,
                                    conf_level = 0.95,
                                    max_iter   = 1000,
                                    quiet      = FALSE) {


  ## calculate Coefficient of Variation (CV)
  cv <- sd(x) / mean(x) * 100

  ## population size
  max_n <- total_area / plot_size

  ## calculate Student's t for selected CI
  students_t <- qt((1 + conf_level) / 2, df = max_n)

  ## calculate n
  n <- calc_n_simple(students_t, max_n, cv, max_error)

  ## if n equals 1 initially, t-test cannot be computed with 1 - 1 df
  if (n == 1) {
    if (!quiet) cli::cli_alert_warning("The estimated sample size is 1. Consider decreasing the sampling error")
    return(n)
  }

  ## initialize values
  final_n <- 0
  i       <- 1

  ## calculate final sample size (when n == final_n, or maximum iterations happens)
  while (n != final_n && i < max_iter) {
    ## calculate new student's t
    new_students_t <- qt((1 + conf_level) / 2, df = max(final_n - 1, 1))
    ## update n
    n <- final_n
    ## calculate new n
    final_n <- calc_n_simple(new_students_t, max_n, cv, max_error)
    i <- i + 1
  }

  ## return warning if no convergence has been found after maximum iterations
  if (i == max_iter) {
    if (!quiet) cli::cli_alert_warning("No convergence found after {max_iter} iterations. Returning latest value.")
    final_n <- min(n, final_n)
  }

  ## return
  ci_lo <- (mean(x) - mean(x) * max_error) |> round(2)
  ci_up <- (mean(x) + mean(x) * max_error) |> round(2)
  effort <- (final_n / total_area * 10000) |> round(2)
  if (!quiet) {
    cli::cli_alert_info("A total of {length(x)} plots were measured in the pilot inventory, each plot measuring {plot_size} squared meters.")
    cli::cli_alert_info("A minimum of {final_n} inventory plots are needed for a maximum sampling error of {max_error * 100}% ({conf_level * 100}% CI [{ci_lo}, {ci_up}]).")
    cli::cli_alert_info("The sampling effort will be {effort} plots/ha")
    cli::cli_alert_info("Note that these calculations assume that you will do a simple random sampling")
  }

  SimpleSampleSize(
    sampling_res = list(
      min_plots       = final_n,
      ci_lo           = ci_lo,
      ci_up           = ci_up,
      sampling_effort = effort
    ),
    sampling_opts = list(
      pilot_plots = x,
      plot_size   = plot_size,
      total_area  = total_area,
      max_error   = max_error,
      conf_level  = conf_level
    )
  )

}












#' Plot an object
#'
#' Generic for plotting objects.
#'
#' @param x Object to plot.
#' @param ... Other arguments passed to methods.
#'
#' @return Usually called for side-effects (producing a plot).
#' @export
plot <- S7::new_generic("plot", "x")




#' Plot Sample Size vs Error
#'
#' This method generates a plot showing how the required sample size varies with the maximum allowed relative error.
#'
#' @param x An object of class `SimpleSampleSize` containing sampling options and results.
#' @param min_error A numeric value specifying the minimum relative error to consider (default is 0.01).
#' @param max_error A numeric value specifying the maximum relative error to consider (default is 0.5).
#'
#' @return A `ggplot` object representing the relationship between error and sample size.
#'
#' @export
S7::method(plot, SimpleSampleSize) <- function(x, min_error = .01, max_error = .5) {

  ## create intervals
  intervals_vec <- seq(min_error, max_error, by = 0.01)

  ## calculate sample size
  ssize <- c()
  for (i in 1:length(intervals_vec)) {

    ith_ssize <- silv_sample_size_simple(
      x          = x@sampling_opts$pilot_plots,
      plot_size  = x@sampling_opts$plot_size,
      total_area = x@sampling_opts$total_area,
      max_error  = intervals_vec[i],
      conf_level = x@sampling_opts$conf_level,
      quiet      = TRUE
    )

    if (inherits(ith_ssize, "S7_object")) {
      ssize <- c(ssize, ith_ssize@sampling_res$min_plots)
    } else {
      ssize <- c(ssize, ith_ssize)
    }


  }

  ## create data frame
  data_df <- data.frame(
    error = intervals_vec,
    ssize = ssize
  )

  ## generate plot
  suppressWarnings({
    data_df |>
      ggplot2::ggplot(
        ggplot2::aes(y = ssize, x = error)
      ) +
      ggplot2::geom_line(
        lwd = 1
      ) +
      ggplot2::annotate(
        "point",
        x = x@sampling_opts$max_error,
        y = x@sampling_res$min_plots,
        color = "#890620",
        size  = 3
      ) +
      ggplot2::labs(
        x = "Relative error",
        y = "Sample size"
      ) +
      ggplot2::theme_bw()
  })

}



