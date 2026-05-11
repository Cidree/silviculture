test_that("eq_hd_vazquez_veloso_2025 adjusts the slope coefficient correctly", {
  generic_row <- silviculture:::h_d_aitor_tbl[
    silviculture:::h_d_aitor_tbl$species_name == "All the species",
    ]

  skip_if(nrow(generic_row) == 0, "Generic row not available in h_d_aitor_tbl")

  model <- eq_hd_vazquez_veloso_2025(
    "All the species",
    bioregion = "atlantic",
    origin = "plantation",
    mixture = "mix"
  )

  expected_b <- generic_row$b + (-0.0691 + 0.0382 - 0.0390)

  expect_equal(model@params$species_params$b, expected_b)
  expect_equal(model@equation, "vazquez_veloso_2025")
  expect_equal(model@species, "All the species")
})
