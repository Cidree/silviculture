test_that("silv_site_factor calculates correctly for Hossfeld II - a", {
  sf <- silv_site_factor("Pinus sylvestris", d0 = 25, h0 = 15)
  expect_equal(sf, 17.57273, tolerance = 1e-4)
})

test_that("silv_site_factor calculates correctly for Hossfeld II - b", {
  sf <- silv_site_factor("Quercus pyrenaica", d0 = 25, h0 = 15)
  expect_equal(sf, 16.5587, tolerance = 1e-4)
})

test_that("silv_site_factor calculates correctly for Bertalanffy-Richards - a", {
  sf <- silv_site_factor("Pinus uncinata", d0 = 25, h0 = 15)
  expect_equal(sf, 17.538, tolerance = 1e-3)
})

test_that("silv_site_factor calculates correctly for Bertalanffy-Richards - b", {
  sf <- silv_site_factor("Quercus suber", d0 = 25, h0 = 10)
  expect_equal(sf, 6.8647, tolerance = 1e-3)
})

test_that("silv_site_factor returns NA with warning for missing species", {
  expect_warning(
    sf <- silv_site_factor("Fake Species", d0 = 25, h0 = 15),
    "not found in Site Factor models"
  )
  expect_true(is.na(sf))
})

test_that("silv_site_factor works with multiple species vectorized", {
  expect_warning(
    sf_multi <- silv_site_factor(
      species = c("Pinus sylvestris", "Quercus suber", "Fake Species"),
      d0 = c(25, 25, 25),
      h0 = c(15, 10, 15)
    ),
    "not found in Site Factor models"
  )
  expect_equal(sf_multi[1], 17.57273, tolerance = 1e-4)
  expect_equal(sf_multi[2], 6.8647, tolerance = 1e-3)
  expect_true(is.na(sf_multi[3]))
})
