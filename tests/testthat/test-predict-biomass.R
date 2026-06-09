test_that("eq_biomass_* custom error messages for BGB and tree gaps work", {
  # Ruiz-Peinado 2011 BGB gap
  expect_error(
    eq_biomass_ruiz_peinado_2011("Abies pinsapo", "BGB"),
    class = "rlang_error",
    regexp = "Model .*ruiz-peinado-2011.* does not include BGB equations for .*Abies pinsapo.*"
  )
  expect_error(
    eq_biomass_ruiz_peinado_2011("Abies pinsapo", "roots"),
    class = "rlang_error",
    regexp = "Model .*ruiz-peinado-2011.* does not include BGB equations for .*Abies pinsapo.*"
  )
  
  # Ruiz-Peinado 2012 BGB gap
  expect_error(
    eq_biomass_ruiz_peinado_2012("Eucalyptus globulus", "BGB"),
    class = "rlang_error",
    regexp = "Model .*ruiz-peinado-2012.* does not include BGB equations for .*Eucalyptus globulus.*"
  )

  # Dieguez-Aranda 2009 tree and BGB gaps
  expect_error(
    eq_biomass_dieguez_aranda_2009("Pinus pinaster", "tree"),
    class = "rlang_error",
    regexp = "Model .*dieguez-aranda-2009.* does not include total-tree.*equations"
  )
  expect_error(
    eq_biomass_dieguez_aranda_2009("Pinus pinaster", "BGB"),
    class = "rlang_error",
    regexp = "Model .*dieguez-aranda-2009.* does not include BGB equations for .*Pinus pinaster.*"
  )

  # Montero 2005 BGB gaps
  expect_error(
    eq_biomass_montero_2005("Abies pinsapo", "BGB"),
    class = "rlang_error",
    regexp = "Model .*montero-2005.* does not include BGB equations for .*Abies pinsapo.*"
  )

  # Manrique 2017 & Cudjoe 2024 gaps
  expect_error(
    eq_biomass_manrique_2017("Quercus petraea", "BGB"),
    class = "rlang_error",
    regexp = "Model .*manrique-2017.* does not include BGB / total-tree equations"
  )
  expect_error(
    eq_biomass_cudjoe_2024("mixed", "tree"),
    class = "rlang_error",
    regexp = "Model .*cudjoe-2024.* does not include BGB / total-tree equations"
  )

  # Test via silv_predict_biomass() wrapper
  expect_error(
    silv_predict_biomass(
      diameter = 20,
      height = 10,
      model = eq_biomass_ruiz_peinado_2011("Abies pinsapo", "BGB")
    ),
    class = "rlang_error",
    regexp = "Model .*ruiz-peinado-2011.* does not include BGB equations for .*Abies pinsapo.*"
  )
})

test_that("silv_predict_biomass_auto priority and height fallback work", {
  # 1. Normal priority selection (Pinus pinaster has height -> ruiz-peinado-2011)
  res_normal <- silv_predict_biomass_auto(
    species = c("Pinus pinaster"),
    diameter = c(20),
    height = c(12),
    component = "tree",
    quiet = TRUE
  )
  expect_equal(res_normal$biomass_model, "ruiz-peinado-2011")
  expect_equal(nrow(res_normal), 1)

  # 2. Height fallback (height is NULL -> montero-2005)
  res_null_h <- silv_predict_biomass_auto(
    species = c("Pinus pinaster"),
    diameter = c(20),
    height = NULL,
    component = "tree",
    quiet = TRUE
  )
  expect_equal(res_null_h$biomass_model, "montero-2005")

  # 3. Height fallback (height is NA/0 -> montero-2005)
  res_na_h <- silv_predict_biomass_auto(
    species = c("Pinus pinaster", "Pinus pinaster"),
    diameter = c(20, 20),
    height = c(NA, 0),
    component = "tree",
    quiet = TRUE
  )
  expect_equal(res_na_h$biomass_model, c("montero-2005", "montero-2005"))

  # 4. Warnings and NA for unsupported combinations
  expect_warning(
    res_unsupported <- silv_predict_biomass_auto(
      species = c("Quercus petraea"),
      diameter = c(25),
      height = c(12),
      component = "tree",
      quiet = FALSE
    ),
    regexp = "No compatible model was found in priority"
  )
  expect_true(is.na(res_unsupported$biomass[1]))
  expect_true(is.na(res_unsupported$biomass_model[1]))
})

test_that("silv_predict_biomass_components works in wide format", {
  # 1. Ruiz-Peinado 2011 (Standard conifer)
  res_peinado <- silv_predict_biomass_components(
    species = c("Pinus pinaster", "Pinus pinaster"),
    diameter = c(20, 25),
    height = c(12, 15),
    model_fn = eq_biomass_ruiz_peinado_2011,
    quiet = TRUE
  )
  expect_equal(nrow(res_peinado), 2)
  expect_true(all(c("species", "diameter", "height", "stem", "thick and medium branches", "small branches and leaves", "roots") %in% colnames(res_peinado)))
  expect_false("tree" %in% colnames(res_peinado))

  # 2. Menendez 2022 (Young plantation model, uses RCD, no component arg)
  res_menendez <- silv_predict_biomass_components(
    species = "Fagus sylvatica",
    diameter = 2,
    height = 3,
    model_fn = eq_biomass_menendez_2022,
    rcd = 2,
    quiet = TRUE
  )
  expect_equal(nrow(res_menendez), 1)
  expect_true("agb" %in% colnames(res_menendez))
  expect_false("roots" %in% colnames(res_menendez))
})

