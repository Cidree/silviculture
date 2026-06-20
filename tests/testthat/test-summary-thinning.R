library(dplyr)

test_that("silv_summary calculates a full inventory summary without errors", {
  df <- inventory_samples |> dplyr::filter(plot_id == 8)
  
  res <- silv_summary(
    data = df,
    diameter = diameter,
    height = height,
    plot_size = 10,
    .groups = c("plot_id", "species")
  )
  
  expect_true(inherits(res, "S7_object"))
  
  gm <- S7::prop(res, "group_metrics")
  expect_true(is.data.frame(gm))
  expect_true(nrow(gm) > 0)
  
  expected_cols <- c("plot_id", "species", "ntrees_ha", "h0", "g_ha", "dg", "spacing", "h_lorey")
  expect_true(all(expected_cols %in% names(gm)))
})

test_that("silv_treatment_thinning simulates thinning correctly", {
  df <- inventory_samples |> 
    dplyr::filter(plot_id == 8) |>
    dplyr::count(species, dclass = silv_tree_dclass(diameter)) |>
    dplyr::mutate(ntrees_ha = silv_density_ntrees_ha(n, plot_size = 10))
    
  res_below <- silv_treatment_thinning(
    data = df,
    var = dclass,
    diameter = dclass,
    ntrees = ntrees_ha,
    thinning = "below",
    perc = 0.3,
    .groups = "species"
  )
  
  expect_true(inherits(res_below, "S7_object"))
  res_data <- S7::prop(res_below, "data")
  
  expect_true(is.data.frame(res_data))
  expect_true("ntrees_ha_extract" %in% names(res_data))
  
  total_orig <- sum(df$ntrees_ha)
  total_extract <- sum(res_data$ntrees_ha_extract)
  expect_true(total_extract > 0)
  
  res_above <- silv_treatment_thinning(
    data = df,
    var = dclass,
    diameter = dclass,
    ntrees = ntrees_ha,
    thinning = "above",
    perc = 0.2,
    .groups = "species"
  )
  
  res_data_above <- S7::prop(res_above, "data")
  total_extract_above <- sum(res_data_above$ntrees_ha_extract)
  expect_true(total_extract_above > 0)
})
