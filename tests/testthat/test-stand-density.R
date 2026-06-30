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


# 3. silv_density_sdimax --------------------------------------------------

test_that("SDImax reference model calculations are correct", {
  # Pinus sylvestris reference: a0 = 12.685, b0 = -1.7524
  # exp(12.685 - 1.7524 * log(25.4)) = 1114.77
  expect_equal(
    silv_density_sdimax("Pinus sylvestris"),
    1114.77,
    tolerance = 0.01
  )

  # Pinus canariensis reference: a0 = 12.672, b0 = -1.8226
  # exp(12.672 - 1.8226 * log(25.4)) = 876.86
  expect_equal(
    silv_density_sdimax("Pinus canariensis"),
    876.86,
    tolerance = 0.01
  )

  # Vectorized calculation
  expect_equal(
    silv_density_sdimax(c("Pinus sylvestris", "Pinus canariensis")),
    c(1114.77, 876.86),
    tolerance = 0.01
  )
})

test_that("SDImax climate-dependent calculations are correct", {
  # Pinus canariensis model P1 with clim_value = 400:
  # a0 = 3.639, a1 = 2.448, b0 = -2.0891, b1 = 0
  # exp((3.639 + 2.448 * log(400)) + (-2.0891) * log(25.4)) = 103600.4
  expect_equal(
    silv_density_sdimax("Pinus canariensis", climatic_model = "P1", clim_value = 400),
    103600.4,
    tolerance = 0.1
  )
})

test_that("SDImax error handling works", {
  # Non-character species
  expect_error(silv_density_sdimax(123))

  # Unsupported species
  expect_error(silv_density_sdimax("Pinus nonexistus"))

  # Missing clim_value when climate model is requested
  expect_error(silv_density_sdimax("Pinus canariensis", climatic_model = "P1"))

  # clim_value provided but climatic_model is basic/NULL
  expect_error(silv_density_sdimax("Pinus canariensis", clim_value = 400))
  expect_error(silv_density_sdimax("Pinus canariensis", climatic_model = "basic", clim_value = 400))

  # Length mismatch between species and clim_value
  expect_error(silv_density_sdimax(c("Pinus canariensis", "Pinus sylvestris"), climatic_model = "P1", clim_value = c(400, 500, 600)))
})

