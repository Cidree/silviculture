test_that("lid_lhdi works correctly", {
  # We construct a simple test vector of z heights
  z <- c(1.1, 1.2, 1.3, 1.6, 1.7, 2.1, 2.2, 2.3)
  
  # By default, interval = 0.5
  # zmin = 1.1 -> floor(1.1 / 0.5) * 0.5 = 1.0
  # Layers:
  # 1.0 to 1.5: 1.1, 1.2, 1.3 (3 points)
  # 1.5 to 2.0: 1.6, 1.7 (2 points)
  # 2.0 to 2.5: 2.1, 2.2, 2.3 (3 points)
  # Total points = 8
  # Proportions (pi): 3/8, 2/8, 3/8
  # LHDI = - (3/8 * log(3/8) + 2/8 * log(2/8) + 3/8 * log(3/8)) = 1.0821955...
  
  expect_equal(lid_lhdi(z), 0.7144, tolerance = 1e-3)
  
  # Test with a different interval
  # interval = 1.0
  # zmin = 1.1 -> 1.0
  # 1.0 to 2.0: 1.1, 1.2, 1.3, 1.6, 1.7 (5 points)
  # 2.0 to 3.0: 2.1, 2.2, 2.3 (3 points)
  # Proportions: 5/8, 3/8
  # LHDI = - (5/8 * log(5/8) + 3/8 * log(3/8)) = 0.6615632...
  
  expect_equal(lid_lhdi(z, interval = 1.0), 0.29375, tolerance = 1e-3)
})

test_that("lid_fcov works correctly", {
  z <- c(1, 4, 6, 8, 2, 7)
  rn <- c(1, 2, 1, 1, 2, 1)
  
  # first returns (rn == 1L):
  # corresponding z values: 1, 6, 8, 7
  # greater than th=5: 6, 8, 7 (3 points)
  # total first returns: 4
  # fcov = 3 / 4 = 0.75
  
  expect_equal(lid_fcov(z, rn, th = 5), 0.75)
  
  # change threshold
  expect_equal(lid_fcov(z, rn, th = 6), 0.50) # 8 and 7 are > 6, so 2 / 4 = 0.50
})
