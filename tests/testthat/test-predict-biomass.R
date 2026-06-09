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
})
