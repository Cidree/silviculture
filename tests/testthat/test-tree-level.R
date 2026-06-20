library(dplyr)

# 1. silv_tree_dclass -------------------------------------------------

## Create data
diameters <- c(5, 7.6, 10.6, 12.5, 31)

## Tests
test_that("Default values work", {
  expect_equal(silv_tree_dclass(diameters), c(NA, 10, 10, 15, 30))
  expect_equal(length(silv_tree_dclass(diameters)), 5)
})

test_that("include lowest work", {
  expect_equal(
    silv_tree_dclass(diameters, include_lowest = FALSE), c(NA, 10, 10, 10, 30)
  )
})

test_that("dmin works", {
  expect_equal(
    silv_tree_dclass(diameters, dmin = 5), c(7.5, 7.5, 12.5, 12.5, 32.5)
  )
})

test_that("dmax works", {
  expect_equal(
    silv_tree_dclass(diameters, dmax = 25), c(NA, 10, 10, 15, 25)
  )
})

test_that("return intervals works", {
  expect_s3_class(
    silv_tree_dclass(diameters, return_intervals = TRUE), "factor"
  )
})


test_that("class length works", {
  expect_equal(
    silv_tree_dclass(diameters, class_length = 10), c(NA, 12.5, 12.5, 12.5, 32.5)
  )

  expect_equal(
    silv_tree_dclass(diameters, dmin = 5, class_length = 10), c(10, 10, 10, 10, 30)
  )
})

test_that("Errors and warnings work", {

  expect_error(silv_tree_dclass(c(-5, 20, 12)))

  expect_error(silv_tree_dclass(diameters, dmin = -1))
  expect_error(silv_tree_dclass(diameters, dmax = -1))
  expect_error(silv_tree_dclass(diameters, class_length = -1))
  expect_error(silv_tree_dclass(diameters, dmin = 5, dmax = 4))
  expect_error(silv_tree_dclass(diameters, dmin = 5, dmax = 5))
})

# 2. silv_tree_basal_area ------------------------------------------------------

## Tests
test_that("Calculating one G works", {
  expect_equal(
    silv_tree_basal_area(60),
    0.2827,
    tolerance = .001
  )
})

test_that("Different units work", {
  expect_equal(
    silv_tree_basal_area(60 / 100, units = "m"),
    0.2827,
    tolerance = .001
  )

  expect_equal(
    silv_tree_basal_area(600, units = "mm"),
    0.2827,
    tolerance = .001
  )
})

## Test errors
test_that("Errors work", {
  expect_error(silv_tree_basal_area(20, units = "km"))
  expect_error(silv_tree_basal_area(diameter = "20"))
})

# 3. silv_tree_volume ------------------------------------------------------

test_that("silv_tree_volume calculates volumes correctly", {
  # Pressler method: V = 2/3 * A_base * h
  # D_base = 20 cm = 0.2 m => A = (pi/4)*0.2^2 = 0.03141593
  # h = 10 m => V = 0.2094395
  v_pressler <- silv_tree_volume(diameter_base = 20, height = 10, formula = "pressler")
  expect_equal(v_pressler, 0.2094395, tolerance = 1e-4)

  # Smalian method: V = (A_base + A_top) / 2 * h
  # D_base = 20, D_top = 10. A_base = 0.03141593, A_top = 0.00785398
  # h = 5 m => V = 0.09817478
  v_smalian <- silv_tree_volume(diameter_base = 20, diameter_top = 10, height = 5, formula = "smalian")
  expect_equal(v_smalian, 0.09817478, tolerance = 1e-4)
})

