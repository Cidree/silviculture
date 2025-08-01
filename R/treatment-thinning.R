




Thinning <- S7::new_class(
  name    = "Thinning",
  package = "silviculture",
  properties = list(
    data          = S7::new_property(S7::class_data.frame, default = quote(data.frame())),
    group_metrics = S7::new_property(),
    thinning_opts = S7::new_property(S7::class_list, default = quote(list()))
  )
)





#' Calculate Forestry Thinning Schemes
#'
#' Calculates thinning schemes for forest management by selecting trees to extract
#' based on specified criteria. Supports both low thinning (removing smaller trees)
#' and high thinning (removing larger trees) approaches.
#'
#' @param data A data frame, or silviculture::Inventory object. See details.
#' @param var A variable used for calculating the thinning. Typically used variables
#' basal area, number of trees, or volume
#' @param dclass Numeric vector with diametric classes
#' @param ntrees Numeric vector with the number of trees per hectare of each diametric
#' class
#' @param thinning Charater string specifying the thinning type. Available options
#' are `low` and `high`
#' @param perc Numeric value between 0 and 1 specifying the percentage of `var`
#' to extract
#' @param .groups A character vector with variables to group by (e.g. plot id, tree
#' species, etc). Ignored when using a `silviculture::Inventory` object
#'
#' @details
#' This function implements common silvicultural thinning practices:
#'
#' **Low Thinning:** Removes trees with the lowest values of the specified
#' variable. This approach typically removes suppressed, damaged, or poor-quality
#' trees, mimicking natural mortality processes.
#'
#' **High Thinning:** Removes trees with the highest values of the specified
#' variable. This approach harvests the most valuable trees while leaving smaller
#' trees to continue growing.
#'
#' The function calculates which trees to extract based on the ranking of the
#' specified variable and the desired thinning percentage. When grouping variables
#' are provided, thinning is calculated separately for each group.
#' 
#' **Using a silviculture::Inventory object**
#' The result of [silv_summary()] can be used as the `data` argument. If so, the `.groups`
#' will be taken from this object, and it will keep the previous data in a new S7 object.
#' 
#' @seealso [silv_summary()]
#'
#' @return A `silviculture::Thinning` object with three items:
#' - **data**: the input data with two new columns
#' - **group_metrics**: it will include the data from the `silviculture::Inventory` object
#' - **thinning_opts**: options used for S7 methods
#' @export
#'
#' @examples
#' # Get summary of inventory data
#' inventory <- inventory_samples |>
#'  silv_summary(
#'    diameter  = diameter,
#'    height    = height,
#'    plot_size = 25,
#'    .groups   = c('plot_id', 'species')
#'  )
#' 
#' ## Basic low thinning removing 30% of trees based on basal area
#' silv_treatment_thinning(
#'   data     = inventory,
#'   var      = g_ha,
#'   dclass   = dclass,
#'   ntrees   = ntrees_ha,
#'   thinning = "low",
#'   perc     = 0.3
#' )
#'
#' ## Basic high thinning removing 20% of trees based on basal area
#' silv_treatment_thinning(
#'   data     = inventory,
#'   var      = g_ha,
#'   dclass   = dclass,
#'   ntrees   = ntrees_ha,
#'   thinning = "high",
#'   perc     = 0.2
#' )
silv_treatment_thinning <- function(data, var, dclass, ntrees, thinning = "low", perc = 0.3, .groups = NULL) {

  ## check for errors
  if (!thinning %in% c("low", "high")) cli::cli_abort("Thinning must be either `low` or `high`.")
  if (perc < 0 | perc > 1) cli::cli_abort("`perc` must be between 0 and 1.")

  ## take CD table if data is silviculture::Inventory
  if (inherits(data, "silviculture::Inventory")) {
    data_tbl <- data@dclass_metrics
    .groups  <- data@groups
  } else {
    data_tbl <- data
  }

  ## how much of the variable (G, ntrees, V...) should we cut?
  thinning_extract <- sum(dplyr::pull(data_tbl, {{ var }})) * perc

  ## new column name
  var_name <- paste0(rlang::as_name(rlang::enquo(var)), "_extract")

  ## sort depending thinning type
  if (thinning == "low") {
    data_tbl <- dplyr::arrange(data_tbl, !!!rlang::syms(.groups), {{ dclass }})
  } else if (thinning == "high") {
    data_tbl <- dplyr::arrange(data_tbl, !!!rlang::syms(.groups), dplyr::desc({{ dclass }}))
  }

  ## calculate thinning
  thinning_tbl <- data_tbl |>
    dplyr::mutate(
      thinning_extract     = sum({{ var }}) * perc,
      cumulative_before    = cumsum(dplyr::lag({{ var }}, default = 0)),
      remaining_to_extract = pmax(0, thinning_extract - cumulative_before),
      {{ var_name }}       := pmin({{ var }}, remaining_to_extract),
      ntrees_ha_extract    = {{ ntrees }} * .data[[var_name]] / {{ var }},
      .by                  = dplyr::all_of(.groups)
    ) |>
    dplyr::select(-c(thinning_extract, cumulative_before, remaining_to_extract))

  ## return S7
  Thinning(
    data          = thinning_tbl,
    group_metrics = if (inherits(data, "silviculture::Inventory")) data@group_metrics else NULL,
    thinning_opts = list(
      var_name   = var_name,
      dclass_name = rlang::as_name(rlang::enquo(dclass)),
      thinning   = thinning,
      percentage = perc,
      groups     = .groups
    )
  )
}


# S7::method(plot, Thinning) <- function(x) {

#   x@data |>
#     ggplot2::ggplot() +
#     ggplot2::geom_bar(
#       aes()
#     )

# }

# ## plot G to extract
# ## plot stacked extract and remain
# ## one plot for extract, another for remaining

# thinning_tbl <- x@data[x@data$ntrees_ha_extract > 0, ]

# thinning_tbl |>
#     ggplot2::ggplot() +
#     ggplot2::geom_col(
#       aes(as.factor(dclass),.data[[x@thinning_opts$var_name]])
#     ) +
#     ggplot2::labs(
#       x = "DBH (cm)",
#       title = paste0(
#         ""
#       )
#     )
