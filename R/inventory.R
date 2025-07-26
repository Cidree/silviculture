


SampleSize <- S7::new_class(
  name    = "SampleSize",
  package = "silviculture",
  properties = list(
    sampling_res  = S7::new_property(S7::class_list, default = quote(list())),
    sampling_opts = S7::new_property(S7::class_list, default = quote(list()))
  )
)





#' Calculates sample size for a random sampling inventory
#'
#' @param x vector of field survey
#' @param plot_size a numeric vector of length one with plot size in squared meters
#' @param total_area total area of the study area in squared meters
#' @param max_error maximum allowed error
#' @param conf_level confidence level
#' @param max_iter maximum number of iteration to find the plot size
#' @param quiet if \code{TRUE}, messages will be supressed
#'
#' @returns SampleSize object
#' @export
#'
#' @importFrom stats qt sd
#'
#' @examples
#' ## pilot inventory measuring 4 plots of 25x25 meters
#' ## total forest area 15 ha
#' ## measured variable (x): basal area per hectare
#' silv_sample_size(
#'   x          = c(33, 37.5, 42, 35.2),
#'   plot_size  = 25 * 25,  # squared plot of 25x25
#'   total_area = 15 * 1e4, # 15 ha
#'   max_error  = 0.05,
#'   conf_level = 0.95,
#'   max_iter   = 100
#' )
silv_sample_size <- function(x,
                             plot_size  = 100,
                             total_area = 150000,
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

  SampleSize(
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
#' @param x An object of class `SampleSize` containing sampling options and results.
#' @param min_error A numeric value specifying the minimum relative error to consider (default is 0.01).
#' @param max_error A numeric value specifying the maximum relative error to consider (default is 0.5).
#'
#' @return A `ggplot` object representing the relationship between error and sample size.
#'
#' @export
S7::method(plot, SampleSize) <- function(x, min_error = .01, max_error = .5) {

  ## create intervals
  intervals_vec <- seq(min_error, max_error, by = 0.01)

  ## calculate sample size
  ssize <- c()
  for (i in 1:length(intervals_vec)) {

    ith_ssize <- silv_sample_size(
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



