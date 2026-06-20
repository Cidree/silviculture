library(dplyr)

# 1. silv_density_ntrees_ha -------------------------------------------------------

## -> Counts number of trees per hectare from number of trees in a plot

## Create data
trees_data <- inventory_samples |>
  filter(plot_id == 8) |>
  count(species) |>
  mutate(
    ntrees_rect = silv_density_ntrees_ha(
      n,
      plot_size  = c(10, 15),
      plot_shape = "rectangular"
    ),
    ntrees_circ = silv_density_ntrees_ha(
      n,
      plot_size  = 10,
      plot_shape = "circular"
    )
  )

## Tests
test_that("ntrees/ha are correctly calculated", {

  expect_equal(
    trees_data$ntrees_rect,
    c(133.33, 533.33, 466.67, 333.33),
    tolerance = .1
  )

  expect_equal(
    trees_data$ntrees_circ,
    c(63.67, 254.65, 222.82, 159.15),
    tolerance = .1
  )

})

test_that("Errors work", {
  expect_error(
    silv_density_ntrees_ha(c(12, 34, 21), plot_shape = "triangular")
  )
  expect_error(
    silv_density_ntrees_ha(c(12, 34, 21), plot_size = 0)
  )
})

# 2. silv_density_hart ---------------------------------------------------

## Tests
test_that("Spacing index is well calculated", {
  expect_equal(
    silv_density_hart(h0 = 17.8, ntrees = 400),
    28.09,
    tolerance = 0.01
  )

  expect_equal(
    silv_density_hart(h0 = c(17.8, 20.5), ntrees = c(400, 450)),
    c(28.09, 22.99),
    tolerance = 0.01
  )

  expect_equal(
    silv_density_hart(h0 = c(17.8, 20.5), ntrees = c(400, 450), "hart-becking"),
    c(30.18, 24.71),
    tolerance = 0.01
  )
})

## Test errors
test_that("Errors work", {
  expect_error(
    silv_density_hart(17.8, 400, "harty-becking")
  )
  expect_error(silv_density_hart("17.8", 400))
  expect_error(silv_density_hart(17.8, "400"))
  expect_error(silv_density_hart(c(17.8, 20.5), 400))
})
