library(dplyr)

# 1. silv_stand_dominant_height -------------------------------------------------

## Data
inventory_data <- inventory_samples |>
  filter(plot_id == 8) |>
  mutate(dclass = silv_tree_dclass(diameter)) |>
  summarise(
    height = mean(height, na.rm = TRUE),
    ntrees = n(),
    .by    = c(plot_id, species, dclass)
  ) |>
 mutate(
   ntrees_ha = silv_density_ntrees_ha(ntrees, plot_size = 10),
   .by       = species
 )

d0_data <- inventory_data |>
  reframe(
    h0_assman = silv_stand_dominant_height(dclass, height, ntrees_ha),
    h0_hart = silv_stand_dominant_height(dclass, height, ntrees_ha, "hart"),
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
    silv_stand_dominant_height(
      inventory_data$dclass,
      inventory_data$height,
      which = "asman"
    )
  )

  expect_error(
    silv_stand_dominant_height(
      c(10, 34, 20),
      c(10, 15, -3)
    )
  )

  expect_error(
    silv_stand_dominant_height(
      c(10, 34, -3),
      c(10, 15, 20)
    )
  )

})

# 2. silv_stand_lorey_height ----------------------------------------------------

## Calculate Lorey's Height
lh_plot <- inventory_samples |>
  filter(plot_id == 8) |>
  mutate(g = silv_tree_basal_area(diameter)) |>
  summarise(
    lh  = silv_stand_lorey_height(height, g),
    .by = species
  )

lh_ha <- inventory_samples |>
  filter(plot_id == 8) |>
  mutate(dclass = silv_tree_dclass(diameter)) |>
  summarise(
    height = mean(height, na.rm = TRUE),
    ntrees = n(),
    .by    = c(plot_id, species, dclass)
  ) |>
  mutate(
    ntrees_ha = silv_density_ntrees_ha(ntrees, plot_size = 10),
    g         = silv_tree_basal_area(dclass),
    .by       = species
  ) |>
  summarise(
    lh  = silv_stand_lorey_height(height, g, ntrees_ha),
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
    c(5.3500, 17.6736, 6.9325, 6.1095),
    tolerance = .001
  )
})

test_that("Errors work", {
  expect_error(silv_stand_lorey_height(c(2, 3, 6), c(10, 23)))

  expect_error(silv_stand_lorey_height(c(2, 3), c(10, 23), c(10)))
})

# 3. silv_stand_qmean_diameter ------------------------------------------------

## Tests
test_that("Squared mean diameter is well calculated", {

  expect_equal(
    silv_stand_qmean_diameter(c(12.5, 23.5, 14, 16, 18.5)),
    17.34,
    tolerance = 0.01
  )

  expect_equal(
    silv_stand_qmean_diameter(
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
    silv_stand_qmean_diameter(
      c(10, 20, 30),
      c(60, 90, 10, 12)
    )
  )
  expect_error(
    silv_stand_qmean_diameter(
      c(-20, 40),
      c(10, 20)
    )
  )
})

# 4. silv_stand_basal_area ------------------------------------------------------

test_that("Calculating G for several trees works", {
  expect_equal(
    silv_stand_basal_area(60, 10),
    0.2827 * 10,
    tolerance = .001
  )
})

test_that("Different units work", {
  expect_equal(
    silv_stand_basal_area(60 / 100, 10, "m"),
    0.2827 * 10,
    tolerance = .001
  )

  expect_equal(
    silv_stand_basal_area(600, 10, "mm"),
    0.2827 * 10,
    tolerance = .001
  )
})

## Test errors
test_that("Errors work", {
  expect_error(silv_stand_basal_area(20, units = "km"))
  expect_error(silv_stand_basal_area(c(20, 40), c(10)))
})
