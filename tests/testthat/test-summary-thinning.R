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
  
  expected_cols <- c("plot_id", "species", "ntrees_ha", "h0", "g_ha", "dg", "spacing", "h_lorey", "slenderness")
  expect_true(all(expected_cols %in% names(gm)))
})

test_that("silv_summary_species calculates top species correctly", {
  df <- inventory_samples |> 
    dplyr::mutate(expan = silv_density_ntrees_ha(1, 10))
    
  res <- silv_summary_species(df, plot_id, species, expan, diameter = diameter, top_n = 3)
  
  expect_true(is.data.frame(res))
  expect_true("sp1" %in% names(res))
  expect_true("G_sp1" %in% names(res))
  expect_true("N_sp1" %in% names(res))
})

test_that("silv_summary_mortality calculates dead vs alive correctly", {
  set.seed(42)
  df <- inventory_samples |> 
    dplyr::mutate(expan = silv_density_ntrees_ha(1, 10)) |>
    dplyr::mutate(is_dead = sample(c(TRUE, FALSE), dplyr::n(), replace = TRUE, prob = c(0.1, 0.9)))
    
  res <- silv_summary_mortality(df, plot_id, is_dead, expan, diameter = diameter)
  
  expect_true(is.data.frame(res))
  expect_true("N_alive" %in% names(res))
  expect_true("G_alive" %in% names(res))
  expect_true("N_dead" %in% names(res))
  expect_true("G_dead" %in% names(res))
})

test_that("silv_summary aggregates and predicts volume, biomass, and carbon", {
  df <- inventory_samples |> dplyr::filter(plot_id == 8, species == 28) |> dplyr::mutate(province = 1)

  # With automatic predictions
  res <- silv_summary(
    data            = df,
    diameter        = diameter,
    height          = height,
    plot_size       = 10,
    .groups         = "plot_id",
    species         = species,
    province        = province,
    predict_volume  = TRUE,
    predict_biomass = TRUE,
    predict_carbon  = TRUE
  )

  gm <- S7::prop(res, "group_metrics")
  expect_true(all(c("v_ha", "w_ha", "c_ha") %in% names(gm)))
  expect_true(gm$v_ha > 0)
  expect_true(gm$w_ha > 0)
  expect_true(gm$c_ha > 0)

  # With pre-calculated columns
  df_pre <- df |>
    dplyr::mutate(
      vol = 150, # dm3
      w   = 100, # kg
      c   = 50   # kg
    )

  res_pre <- silv_summary(
    data      = df_pre,
    diameter  = diameter,
    height    = height,
    plot_size = 10,
    .groups   = "plot_id",
    volume    = vol,
    biomass   = w,
    carbon    = c
  )
  gm_pre <- S7::prop(res_pre, "group_metrics")
  expect_true(all(c("v_ha", "w_ha", "c_ha") %in% names(gm_pre)))
})

test_that("silv_summary_species and silv_summary_mortality support volume, biomass, carbon", {
  set.seed(42)
  df <- inventory_samples |>
    dplyr::mutate(
      expan   = silv_density_ntrees_ha(1, 10),
      is_dead = sample(c(TRUE, FALSE), dplyr::n(), replace = TRUE, prob = c(0.1, 0.9)),
      vol     = 100,
      w       = 80,
      c       = 40
    )

  res_sp <- silv_summary_species(
    df, plot_id, species, expan,
    diameter = diameter, volume = vol, biomass = w, carbon = c, top_n = 3
  )
  expect_true(all(c("V_sp1", "W_sp1", "C_sp1") %in% names(res_sp)))

  res_mort <- silv_summary_mortality(
    df, plot_id, is_dead, expan,
    diameter = diameter, volume = vol, biomass = w, carbon = c
  )
  expect_true(all(c("V_alive", "V_dead", "W_alive", "W_dead", "C_alive", "C_dead") %in% names(res_mort)))
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
