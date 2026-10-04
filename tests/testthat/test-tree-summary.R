library(dplyr)

test_that("silv_tree_summary calculates basic tree metrics correctly", {
  df <- inventory_samples |> dplyr::filter(plot_id == 8)

  res <- silv_tree_summary(
    data        = df,
    diameter    = diameter,
    height      = height,
    plot_id     = plot_id,
    species     = species,
    plot_size   = 10,
    compute_bal = TRUE
  )

  expect_true(is.data.frame(res))
  expect_true(all(c("dclass", "g", "expan", "g_ha", "slenderness", "bal", "bas") %in% names(res)))
  expect_equal(nrow(res), nrow(df))
  expect_true(all(res$g > 0))
  expect_true(all(res$slenderness > 0))
  expect_true(all(res$bal >= 0))
})

test_that("silv_tree_summary handles biomass and carbon predictions", {
  df <- inventory_samples |> dplyr::filter(plot_id == 8)

  res <- silv_tree_summary(
    data            = df,
    diameter        = diameter,
    height          = height,
    species         = species,
    plot_size       = 10,
    predict_biomass = TRUE,
    predict_carbon  = TRUE
  )

  expect_true(is.data.frame(res))
  expect_true("biomass" %in% names(res))
  expect_true("carbon" %in% names(res))
  expect_true(all(!is.na(res$biomass)))
  expect_true(all(!is.na(res$carbon)))
})

test_that("silv_tree_summary handles volume predictions", {
  df <- inventory_samples |> dplyr::filter(plot_id == 8, species == 28) |> dplyr::mutate(province = 1)

  res <- silv_tree_summary(
    data           = df,
    diameter       = diameter,
    height         = height,
    species        = species,
    province       = province,
    predict_volume = TRUE
  )

  expect_true(is.data.frame(res))
  expect_true(all(c("vcc", "vsc", "iavc") %in% names(res)))
})

test_that("silv_tree_summary errors work properly", {
  df <- inventory_samples |> dplyr::filter(plot_id == 8)

  expect_error(silv_tree_summary(df))
  expect_error(silv_tree_summary(df, diameter = diameter, compute_bal = TRUE)) # missing plot_id
  expect_error(silv_tree_summary(df, diameter = diameter, predict_volume = TRUE)) # missing species and province
})
