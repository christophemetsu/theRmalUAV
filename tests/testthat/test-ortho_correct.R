test_that("ortho_correct errors on invalid input types", {
  # Invalid input: not a path or SpatRaster
  expect_error(ortho_correct(thermal_ortho = 123),
               "thermal_ortho should either be a path to a tif or a SpatRaster")

  # Invalid temperature unit
  r <- terra::rast(nrows=2, ncols=2, vals=25)
  terra::crs(r) <- "EPSG:4326"
  expect_error(ortho_correct(r, temp = "X", flight_height = 10),
               "Invalid value for temp")

  # Missing flight height
  expect_error(ortho_correct(r, temp = "C"),
               "flight_heighth not found")

  # rel_hum out of range
  expect_error(ortho_correct(r, temp = "C", flight_height = 10, rel_hum = 200))

  # Invalid emissivity
  expect_error(ortho_correct(r, temp = "C", flight_height = 10, emiss = 2))
})

test_that("ortho_correct returns output with correct layer name", {
  r <- terra::rast(nrows=2, ncols=2, vals=30)
  terra::crs(r) <- "EPSG:4326"
  result <- ortho_correct(r, temp = "C", flight_height = 10, T_air = 25)

  expect_s4_class(result, "SpatRaster")
  expect_true("Ts" %in% names(result))
  expect_equal(round(result[1][1,],4), 30.5083)
})
