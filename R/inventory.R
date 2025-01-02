

#
# silv_sample_size <- function(x,
#                              plot_size  = 100,
#                              total_area = 15,
#                              max_error  = 0.05,
#                              conf_level = 0.95,
#                              quiet      = FALSE) {
#
#
#   ## calculate Coefficient of Variation (CV)
#   cv <- sd(x) / mean(x) * 100
#
#   ## population size
#   max_n <- total_area * 10000 / plot_size
#
#   ## calculate Student's t for selected CI
#   students_t <- qt((1 + conf_level) / 2, df = max_n)
#
#   ## calculate n
#   n <- calc_n_simple(students_t, max_n, cv, max_error)
#
#   ## if n equals 1 initially, t-test cannot be computed with 1 - 1 df
#   if (n == 1) {
#     warning("The estimated sample size is 1. Consider decreasing the sampling error")
#     return(n)
#   }
#
#   ## iterate to find n samples
#   for (i in 2:1000) {
#
#     ## calculate new student's t
#     new_students_t <- qt((1 + conf_level) / 2, df = max(n[i - 1] - 1, 1))
#
#     ## calculate new n
#     new_n <- calc_n_simple(new_students_t, max_n, cv, max_error)
#
#     ## add to vector
#     n <- c(n, new_n)
#
#     ## break when previous n and new n are the same or;
#     ## when we enter in an infinite loop
#     if (new_n == n[i - 1] || (i > 2 && new_n == n[i - 2])) {
#       final_n <- new_n
#       break
#     }
#
#   }
#
#   ## return warning if no convergence has been found after 1000 iterations
#   if (i == 1000) {
#     warning("No convergence found after 1000 iterations. Returning latest value.")
#     final_n <- n[1000]
#   }
#
#   ## return
#   ci_lo <- (mean(x) - mean(x) * max_error) |> round(2)
#   ci_up <- (mean(x) + mean(x) * max_error) |> round(2)
#   effort <- (final_n / total_area) |> round(2)
#   if (!quiet) {
#     message(
#       glue::glue("A total of {length(x)} plots were measured in the pilot inventory, each plot measuring {plot_size} squared meters.
#                A minimum of {final_n} inventory plots are needed for a maximum sampling error of {max_error * 100}% ({conf_level * 100}% CI [{ci_lo}, {ci_up}]).
#                The sampling effort will be {effort} plots/ha
#                Note that these calculations assume that you will do a simple random sampling")
#     )
#   }
#   return(final_n)
#
# }
#
#
# silv_plot_sample_size <- function(x,
#                                   min_error = 0.01,
#                                   max_error = 0.30,
#                                   plot_size  = 100,
#                                   total_area = 15,
#                                   conf_level = 0.95,
#                                   log_scale  = FALSE) {
#
#   ## create intervals
#   intervals_vec <- seq(min_error, max_error, by = 0.01)
#
#   ## calculate sample size
#   ssize <- c()
#   for (i in 1:length(intervals_vec)) {
#
#     ith_ssize <- silv_sample_size(
#       x          = x,
#       plot_size  = plot_size,
#       total_area = total_area,
#       max_error  = intervals_vec[i],
#       conf_level = conf_level,
#       quiet      = TRUE
#     )
#
#     ssize <- c(ssize, ith_ssize)
#
#   }
#
#   ## create data frame
#   data_df <- data.frame(
#     error = intervals_vec,
#     ssize = ssize
#   )
#
#   ## generate base plot
#   base_gg <- data_df |>
#     ggplot(
#       aes(y = ssize, x = error)
#     ) +
#     geom_line(
#       lwd = 1
#     ) +
#     labs(
#       x = "Relative error",
#       y = "Sample size"
#     ) +
#     theme_bw()
#
#   ## return plot based on log argument
#   if (log_scale) {
#     base_gg +
#       scale_y_log10()
#   } else {
#     base_gg
#   }
#
#
# }



