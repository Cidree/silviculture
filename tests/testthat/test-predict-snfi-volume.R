test_that("silv_predict_snfi_volume returns correct structure", {
  res <- silv_predict_snfi_volume(province = 1, species = 21,
                                  dbh = 20, h = 15, dnm = 23, quiet = TRUE)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1L)
  expect_named(res, c("vcc", "vsc", "iavc", "vle", "snfi_version"))
})

test_that("silv_predict_snfi_volume SNFI4 numeric province + species", {
  res <- silv_predict_snfi_volume(province = 1, species = 21,
                                  dbh = 20, h = 15, dnm = 23, quiet = TRUE)
  expect_equal(res$vcc,  209.6633, tolerance = 1e-3)
  expect_equal(res$vsc,  162.8855, tolerance = 1e-3)
  expect_equal(res$iavc, 7.07152,  tolerance = 1e-3)
  expect_equal(res$vle,  10.37648, tolerance = 1e-3)
  expect_equal(res$snfi_version, "SNFI4")
})

test_that("silv_predict_snfi_volume SNFI3 numeric province + species", {
  res <- silv_predict_snfi_volume(province = 12, species = 65,
                                  dbh = 20, h = 15, dnm = 23, quiet = TRUE)
  expect_equal(res$vcc,  166.5862, tolerance = 1e-3)
  expect_equal(res$vsc,  136.6675, tolerance = 1e-3)
  expect_equal(res$iavc, 1.282315, tolerance = 1e-4)
  expect_equal(res$vle,  19.35512, tolerance = 1e-3)
  expect_equal(res$snfi_version, "SNFI3")
})

test_that("silv_predict_snfi_volume accepts character province and species", {
  res_chr <- silv_predict_snfi_volume(province = "Cantabria",
                                      species  = "Pinus radiata",
                                      dbh = 20, h = 15, dnm = 23, quiet = TRUE)
  res_num <- silv_predict_snfi_volume(province = 39, species = 28,
                                      dbh = 20, h = 15, dnm = 23, quiet = TRUE)
  expect_equal(res_chr$vcc,  res_num$vcc)
  expect_equal(res_chr$vsc,  res_num$vsc)
  expect_equal(res_chr$iavc, res_num$iavc)
  expect_equal(res_chr$vle,  res_num$vle)
  expect_equal(res_chr$snfi_version, res_num$snfi_version)
})

test_that("silv_predict_snfi_volume Cantabria / Pinus radiata reference values", {
  res <- silv_predict_snfi_volume(province = "Cantabria",
                                  species  = "Pinus radiata",
                                  dbh = 20, h = 15, dnm = 23, quiet = TRUE)
  expect_equal(res$vcc,  198.92,   tolerance = 1e-3)
  expect_equal(res$vsc,  154.2178, tolerance = 1e-3)
  expect_equal(res$iavc, 13.60531, tolerance = 1e-3)
  expect_equal(res$vle,  21.08256, tolerance = 1e-3)
  expect_equal(res$snfi_version, "SNFI4")
})

test_that("silv_predict_snfi_volume vectorized input returns 2 rows", {
  res <- silv_predict_snfi_volume(
    province = c(1, 12), species = c(21, 65),
    dbh = c(20, 20), h = c(15, 15), dnm = c(23, 23),
    quiet = TRUE
  )
  expect_equal(nrow(res), 2L)
  expect_equal(res$snfi_version, c("SNFI4", "SNFI3"))
  expect_equal(res$vcc[1], 209.6633, tolerance = 1e-3)
  expect_equal(res$vcc[2], 166.5862, tolerance = 1e-3)
})

test_that("silv_predict_snfi_volume scalar inputs recycled to match length", {
  res_scalar <- silv_predict_snfi_volume(
    province = 1, species = 21, dbh = 20, h = 15, dnm = 23, quiet = TRUE
  )
  res_scalar2 <- silv_predict_snfi_volume(
    province = c(1, 1), species = c(21, 21),
    dbh = c(20, 20), h = c(15, 15), dnm = c(23, 23), quiet = TRUE
  )
  r1 <- as.list(res_scalar2[1, ]); r2 <- as.list(res_scalar2[2, ])
  expect_equal(r1$vcc, r2$vcc); expect_equal(r1$vsc, r2$vsc)
  expect_equal(r1$iavc, r2$iavc); expect_equal(r1$vle, r2$vle)
  expect_equal(r1$snfi_version, r2$snfi_version)
})

test_that("silv_predict_snfi_volume: unknown province triggers error", {
  expect_error(
    silv_predict_snfi_volume(province = "Narnia", species = 21,
                             dbh = 20, h = 15, quiet = TRUE),
    class = "rlang_error"
  )
})

test_that("silv_predict_snfi_volume: unknown species produces warning and NA", {
  expect_warning(
    res <- silv_predict_snfi_volume(province = 1, species = 9999,
                                   dbh = 20, h = 15, quiet = TRUE)
  )
  expect_true(is.na(res$vcc))
  expect_true(is.na(res$vsc))
})

test_that("silv_predict_snfi_volume: quiet = TRUE suppresses citation messages", {
  expect_no_message(
    silv_predict_snfi_volume(province = 1, species = 21,
                             dbh = 20, h = 15, dnm = 23, quiet = TRUE)
  )
})

test_that("silv_snfi_provinces returns expected structure", {
  prov <- silv_snfi_provinces()
  expect_s3_class(prov, "data.frame")
  expect_named(prov, c("province_id", "province_name", "snfi_version"))
  expect_true(all(prov$snfi_version %in% c("SNFI4", "SNFI3")))
  expect_gte(nrow(prov), 50L)
})

test_that("silv_snfi_species SNFI4 returns expected structure", {
  sp <- silv_snfi_species("SNFI4")
  expect_s3_class(sp, "data.frame")
  expect_named(sp, c("species_code", "species_name"))
  expect_gt(nrow(sp), 0L)
})

test_that("silv_snfi_species SNFI3 returns expected structure", {
  sp <- silv_snfi_species("SNFI3")
  expect_s3_class(sp, "data.frame")
  expect_named(sp, c("species_code", "species_name"))
  expect_gt(nrow(sp), 0L)
})

test_that("silv_snfi_provinces: province 1 is SNFI4, province 12 is SNFI3", {
  prov <- silv_snfi_provinces()
  expect_equal(prov$snfi_version[prov$province_id == 1],  "SNFI4")
  expect_equal(prov$snfi_version[prov$province_id == 12], "SNFI3")
})

# Complete Combinatorial Grid Test for Robustness
test_that("silv_predict_snfi_volume handles full combination grid", {
  provinces <- c(1, 12, 39) # 1 and 39 are SNFI4, 12 is SNFI3 fallback
  species_codes <- c(21, 28, 65) # Pinus sylvestris, Pinus radiata, Fagus sylvatica
  qualities <- c("default", 2, 5)

  grid <- expand.grid(
    province = provinces,
    species = species_codes,
    quality = qualities,
    stringsAsFactors = FALSE
  )

  # Run all combinations
  expect_no_error({
    res <- silv_predict_snfi_volume(
      province = grid$province,
      species  = grid$species,
      dbh      = 22,
      h        = 16,
      dnm      = 24,
      quality  = grid$quality,
      quiet    = TRUE
    )
    expect_equal(nrow(res), nrow(grid))
    expect_named(res, c("vcc", "vsc", "iavc", "vle", "snfi_version"))
    # Ensure there are no runtime crashes or class errors
    expect_true(is.numeric(res$vcc) || all(is.na(res$vcc)))
    expect_true(is.numeric(res$vsc) || all(is.na(res$vsc)))
  })
})
