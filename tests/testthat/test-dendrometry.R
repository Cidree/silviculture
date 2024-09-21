

library(dplyr)

# 1. silv_diametric class -------------------------------------------------

## Create data
diameters <- c(5, 7.6, 10.6, 12.5, 31)

## Tests
test_that("Default values work", {
  expect_equal(silv_diametric_class(diameters), c(NA, 10, 10, 15, 30))
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
    silv_diametric_class(diameters, dmax = 25), c(NA, 10, 10, 10, 25)
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

# 3. silv_dominant_height -------------------------------------------------

## Data
d0_data <- inventory_samples |>
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
 ) |>
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

# 5. silv_sqrmean_diameter ------------------------------------------------


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



























