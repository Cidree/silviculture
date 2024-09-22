

library(dplyr)

# 1. silv_diametric class -------------------------------------------------

## Create data
diameters <- c(5, 7.6, 10.6, 12.5, 31)

## Tests
test_that("Default values work", {
  expect_equal(silv_diametric_class(diameters), c(NA, 10, 10, 15, 30))
  expect_equal(length(silv_diametric_class(diameters)), 5)
})

test_that("include lowest work", {
  expect_equal(
    silv_diametric_class(diameters, include_lowest = FALSE), c(NA, 10, 10, 10, 30)
  )
})

test_that("dmin works", {
  expect_equal(
    silv_diametric_class(diameters, dmin = 5), c(7.5, 7.5, 12.5, 12.5, 32.5)
  )
})

test_that("dmax works", {
  expect_equal(
    silv_diametric_class(diameters, dmax = 25), c(NA, 10, 10, 15, 25)
  )
})

test_that("return intervals works", {
  expect_s3_class(
    silv_diametric_class(diameters, return_intervals = TRUE), "factor"
  )
})


test_that("class length works", {
  expect_equal(
    silv_diametric_class(diameters, class_length = 10), c(NA, 12.5, 12.5, 12.5, 32.5)
  )

  expect_equal(
    silv_diametric_class(diameters, dmin = 5, class_length = 10), c(10, 10, 10, 10, 30)
  )
})

test_that("Errors and warnings work", {

  expect_warning(silv_diametric_class(c(-5, 20, 12)))

  expect_error(silv_diametric_class(diameters, dmin = -1))
  expect_error(silv_diametric_class(diameters, dmax = -1))
  expect_error(silv_diametric_class(diameters, class_length = -1))
  expect_error(silv_diametric_class(diameters, dmin = 5, dmax = 4))
  expect_error(silv_diametric_class(diameters, dmin = 5, dmax = 5))
})


# 2. silv_ntrees_ha -------------------------------------------------------

## -> Counts number of trees per hectare from number of trees in a plot

## Create data
trees_data <- inventory_samples |>
  filter(plot_id == 8) |>
  count(species) |>
  mutate(
    ntrees_rect = silv_ntrees_ha(
      n,
      plot_size  = c(10, 15),
      plot_shape = "rectangular"
    ),
    ntrees_circ = silv_ntrees_ha(
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
    silv_ntrees_ha(c(12, 34, 21), plot_shape = "triangular")
  )
  expect_error(
    silv_ntrees_ha(c(12, 34, 21), plot_size = 0)
  )
})

# 3. silv_dominant_height -------------------------------------------------

## Data
inventory_data <- inventory_samples |>
  filter(plot_id == 8) |>
  mutate(dclass = silv_diametric_class(diameter)) |>
  summarise(
    height = mean(height, na.rm = TRUE),
    ntrees = n(),
    .by    = c(plot_id, species, dclass)
  ) |>
 mutate(
   ntrees_ha = silv_ntrees_ha(ntrees, plot_size = 10),
   .by       = species
 )

d0_data <- inventory_data |>
  reframe(
    h0_assman = silv_dominant_height(dclass, height, ntrees_ha),
    h0_hart = silv_dominant_height(dclass, height, ntrees_ha, "hart"),
    .by = species
  ) |>
  distinct()


## Tests
test_that("dominant height is well calculated", {

  expect_equal(
    d0_data$h0_assman,
    c(5.14, 17.50, 7.12, 6.39),
    tolerance = .001
  )

  expect_equal(
    d0_data$h0_hart,
    c(6.10, 17.50, 7.125, 6.80),
    tolerance = .001
  )

})

## Test errors
test_that("Errors and warnings work", {
  expect_error(
    silv_dominant_height(
      inventory_data$dclass,
      inventory_data$height,
      which = "asman"
    )
  )

  expect_warning(
    silv_dominant_height(
      c(10, 34, 20),
      c(10, 15, -3)
    )
  )

  expect_warning(
    silv_dominant_height(
      c(10, 34, -3),
      c(10, 15, 20)
    )
  )

})

# 4. silv_lorey_height ----------------------------------------------------

## Calculate Lorey's Height
lh_plot <- inventory_samples |>
  filter(plot_id == 8) |>
  mutate(g = silv_basal_area(diameter)) |>
  summarise(
    lh  = silv_lorey_height(height, g),
    .by = species
  )

lh_ha <- inventory_samples |>
  filter(plot_id == 8) |>
  mutate(dclass = silv_diametric_class(diameter)) |>
  summarise(
    height = mean(height, na.rm = TRUE),
    ntrees = n(),
    .by    = c(plot_id, species, dclass)
  ) |>
  mutate(
    ntrees_ha = silv_ntrees_ha(ntrees, plot_size = 10),
    g         = silv_basal_area(dclass, ntrees_ha),
    .by       = species
  ) |>
  summarise(
    lh  = silv_lorey_height(height, g, ntrees_ha),
    .by = species
  )

## Tests
test_that("Lorey's height by plot", {
  expect_equal(
    lh_plot$lh,
    c(5.46, 17.74, 6.95, 6.14),
    tolerance = .001
  )
})

test_that("Lorey's height by hectare", {
  expect_equal(
    lh_ha$lh,
    c(5.41, 17.67, 7.07, 6.43),
    tolerance = .001
  )
})

test_that("Errors work", {
  expect_error(silv_lorey_height(c(2, 3, 6), c(10, 23)))

  expect_error(silv_lorey_height(c(2, 3), c(10, 23), c(10)))
})

# 5. silv_sqrmean_diameter ------------------------------------------------

## Tests
test_that("Squared mean diameter is well calculated", {

  expect_equal(
    silv_sqrmean_diameter(c(12.5, 23.5, 14, 16, 18.5)),
    17.34,
    tolerance = 0.01
  )

  expect_equal(
    silv_sqrmean_diameter(
      diameter = c(12.5, 23.5, 14, 16, 18.5),
      ntrees   = c(10, 24, 12, 33, 12)
    ),
    18.07,
    tolerance = 0.01

  )

})

## Test errors

test_that("Errors and warnings work", {
  expect_error(
    silv_sqrmean_diameter(
      c(10, 20, 30),
      c(60, 90, 10, 12)
    )
  )
  expect_warning(
    silv_sqrmean_diameter(
      c(-20, 40),
      c(10, 20)
    )
  )
})

# 6. silv_basal_area ------------------------------------------------------

## Tests
test_that("Calculating one G works", {
  expect_equal(
    silv_basal_area(60),
    0.2827,
    tolerance = .001
  )
})

test_that("Calculating G for several trees works", {
  expect_equal(
    silv_basal_area(60, 10),
    0.2827 * 10,
    tolerance = .001
  )
})

test_that("Different units work", {
  expect_equal(
    silv_basal_area(60 / 100, 10, "m"),
    0.2827 * 10,
    tolerance = .001
  )

  expect_equal(
    silv_basal_area(600, 10, "mm"),
    0.2827 * 10,
    tolerance = .001
  )
})

## Test errors
test_that("Errors work", {
  expect_error(silv_basal_area(20, units = "dm"))
  expect_error(silv_basal_area(c(20, 40), c(10)))
  expect_error(silv_basal_area(diameter = "20"))
})

# 7. silv_spacing_index ---------------------------------------------------

## Tests
test_that("Spacing index is well calculated", {
  expect_equal(
    silv_spacing_index(h0 = 17.8, ntrees = 400),
    28.09,
    tolerance = 0.01
  )

  expect_equal(
    silv_spacing_index(h0 = c(17.8, 20.5), ntrees = c(400, 450)),
    c(28.09, 22.99),
    tolerance = 0.01
  )

  expect_equal(
    silv_spacing_index(h0 = c(17.8, 20.5), ntrees = c(400, 450), "hart-becking"),
    c(30.18, 24.71),
    tolerance = 0.01
  )
})

## Test errors
test_that("Errors work", {
  expect_error(
    silv_spacing_index(17.8, 400, "harty-becking")
  )
  expect_error(silv_spacing_index("17.8", 400))
  expect_error(silv_spacing_index(17.8, "400"))
  expect_error(silv_spacing_index(c(17.8, 20.5), 400))
})









