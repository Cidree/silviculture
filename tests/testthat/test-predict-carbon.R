test_that("silv_predict_carbon works correctly", {
  # 1. Exact match Montero 2005
  res1 <- silv_predict_carbon(
    biomass = 100,
    species = "Pinus pinaster",
    component = "stem",
    model = "montero-2005",
    quiet = TRUE
  )
  # montero-2005 pinus pinaster stem carbon_percentage is 51.1
  expect_equal(res1, 100 * (51.1 / 100))
  
  # 2. Unsupported species in Montero 2005 returns NA
  expect_message(
    res2 <- silv_predict_carbon(
      biomass = 100,
      species = "Quercus inventatus",
      component = "stem",
      model = "montero-2005",
      quiet = FALSE
    ),
    regexp = "does not support species"
  )
  expect_true(is.na(res2))
  
  # 3. Vectorized input
  res3 <- silv_predict_carbon(
    biomass = c(100, 200),
    species = c("Pinus pinaster", "Pinus sylvestris"),
    component = c("stem", "roots"),
    model = "montero-2005",
    quiet = TRUE
  )
  expect_length(res3, 2)
})

test_that("silv_predict_carbon_auto works and handles fallbacks", {
  # 1. Exact match from dieguez-aranda-2009 (highest priority by default)
  res_auto1 <- silv_predict_carbon_auto(
    biomass = 100,
    species = "Pinus pinaster",
    component = "stem",
    quiet = TRUE
  )
  expect_equal(nrow(res_auto1), 1)
  expect_equal(res_auto1$carbon_model[1], "montero-2005")
  expect_equal(res_auto1$carbon[1], 100 * (51.1 / 100))

  # 2. Fallback to genus
  expect_message(
    res_auto2 <- silv_predict_carbon_auto(
      biomass = 100,
      species = "Eucalyptus inventatus",
      component = "tree",
      quiet = FALSE
    ),
    regexp = "Using genus fallback"
  )
  expect_equal(res_auto2$carbon_model[1], "montero-2005 (genus fallback)")
  
  # 3. Completely unsupported returns NA and warns
  expect_message(
    res_auto3 <- silv_predict_carbon_auto(
      biomass = 100,
      species = "Inventatus absurdus",
      component = "stem",
      quiet = FALSE
    ),
    regexp = "Could not find carbon percentage.*Otras con\u00edferas"
  )
  expect_true(is.na(res_auto3$carbon[1]))
})
