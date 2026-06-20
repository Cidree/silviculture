test_that("silv_sample_size_simple works and returns correct object", {
  # dummy pilot inventory data
  x <- c(10, 12, 15, 11, 14, 13, 10, 11)
  
  # plot_size = 500 m^2, total_area = 10 ha
  res <- silv_sample_size_simple(x, plot_size = 500, total_area = 10, quiet = TRUE)
  
  # check that it doesn't fail and returns an S7 object
  expect_true(is.numeric(res))
  
  # test an error condition: negative area
  expect_error(silv_sample_size_simple(x, plot_size = 500, total_area = -10, quiet = TRUE))
})

test_that("silv_sample_size_stratified works and returns correct object", {
  # dummy pilot data
  df <- data.frame(
    strata_id = rep(c("A", "B"), each = 4),
    vol = c(10, 12, 15, 11, 20, 22, 25, 23),
    area = rep(c(5, 10), each = 4)
  )
  
  res <- silv_sample_size_stratified(
    data = df,
    x = vol,
    strata = strata_id,
    total_area = area,
    plot_size = 500,
    quiet = TRUE
  )
  
  # check that it doesn't fail and returns an S7 object
  expect_true(is.data.frame(res))
  
  # test method proportional
  res_prop <- silv_sample_size_stratified(
    data = df,
    x = vol,
    strata = strata_id,
    total_area = area,
    plot_size = 500,
    method = "prop",
    quiet = TRUE
  )
  expect_true(is.data.frame(res_prop))
})
