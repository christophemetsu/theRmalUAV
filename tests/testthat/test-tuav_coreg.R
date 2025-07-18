test_that("tuav_coreg throws error with wrong class", {
  expect_error(tuav_coreg(list()),
               regexp = "not of class ThermalUAV")
})

test_that("tuav_coreg throws error with missing opt_cameras", {
  test_thermaluav <- tuav_create(path = "../Test_image/", camera = "DJI_M3T")
  expect_error(tuav_coreg(test_thermaluav),
               regexp = "opt_cameras not found")
})
