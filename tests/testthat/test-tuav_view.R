test_that("tuav_view throws error for wrong class", {
  expect_error(tuav_view(list(a = 1)),
               regexp = "not of class ThermalUAV")
})

test_that("tuav_view returns plot for GPS positions", {
  test_thermaluav <- tuav_create(path = "../Test_image/", camera = "DJI_M3T")
  result <- tuav_loc(test_thermaluav, extent = FALSE)
  p <- tuav_view(result, extent = FALSE)
  expect_true(inherits(p, "leaflet"))  # terra::plet returns a leaflet object
})

test_that("tuav_view returns plot for extents", {
  test_thermaluav <- tuav_create(path = "../Test_image/", camera = "DJI_M3T")
  result <- tuav_loc(test_thermaluav, extent = TRUE)
  p <- tuav_view(result, extent = TRUE)
  expect_true(inherits(p, "leaflet"))
})
