test_that("tuav_emis throws error for invalid thermal_uav", {
  dummy_raster <- terra::rast(matrix(300, 10, 10))
  expect_error(tuav_emis(dummy_raster, list(), corrmap = dummy_raster),
               "not of class ThermalUAV")
})

test_that("tuav_emis throws error for invalid method", {
  dummy_raster <- terra::rast(matrix(300, 10, 10))
  test <- tuav_create(path = "../Test_image/DJI_20240806173534_0039_T.JPG", camera = "DJI_M3T")

  expect_error(
    tuav_emis(dummy_raster, test, method = "wrong", corrmap = dummy_raster),
    "not provided correctly"
  )
})

test_that("tuav_emis works with NDVI method and returns raster", {
  dummy_raster <- terra::rast(matrix(300, 10, 10))
  terra::crs(dummy_raster) <- "EPSG:4326"
  dummy_ndvi <- terra::rast(matrix(0.6, 10, 10))
  terra::crs(dummy_ndvi) <- "EPSG:4326"

  test <- tuav_create(path = "../Test_image/DJI_20240806173534_0039_T.JPG", camera = "DJI_M3T")
  test_correct <- tuav_correct(test)

  result <- tuav_emis(thermal_orig = dummy_raster,
                      thermal_uav = test_correct,
                      temp = "K",
                      corrmap = dummy_ndvi,
                      method = "NDVI")

  expect_s4_class(result, "SpatRaster")
  expect_equal(dim(result), c(10, 10, 1))
  expect_equal(round(result[1][1,],4), 300.3172)
})

test_that("tuav_emis stops if LC_emiss_matrix is not supplied with LC method", {
  dummy_raster <- terra::rast(matrix(300, 10, 10))
  terra::crs(dummy_raster) <- "EPSG:4326"
  dummy_lc <- terra::rast(matrix(1, 10, 10))
  terra::crs(dummy_lc) <- "EPSG:4326"

  test <- tuav_create(path = "../Test_image/DJI_20240806173534_0039_T.JPG", camera = "DJI_M3T")
  test_correct <- tuav_correct(test)

  expect_error(
    tuav_emis(thermal_orig = dummy_raster,
              thermal_uav = test_correct,
              corrmap = dummy_lc,
              method = "LC"),
    "Please provide the parameter LC_emiss_matrix"
  )
})

test_that("tuav_emis works with method LC if correct matrix is provided", {
  dummy_raster <- terra::rast(matrix(300, 10, 10))
  terra::crs(dummy_raster) <- "EPSG:4326"
  dummy_lc <- terra::rast(matrix(1, 10, 10))
  terra::crs(dummy_lc) <- "EPSG:4326"
  lc_matrix <- matrix(c(1, 0.97), ncol = 2)

  test <- tuav_create(path = "../Test_image/DJI_20240806173534_0039_T.JPG", camera = "DJI_M3T")
  test_correct <- tuav_correct(test)

  result <- tuav_emis(thermal_orig = dummy_raster,
                      thermal_uav = test_correct,
                      corrmap = dummy_lc,
                      method = "LC",
                      LC_emiss_matrix = lc_matrix)

  expect_s4_class(result, "SpatRaster")
})

test_that("tuav_emis handles EM method with valid emissivity map", {
  dummy_raster <- terra::rast(matrix(300, 10, 10))
  terra::crs(dummy_raster) <- "EPSG:4326"
  dummy_em <- terra::rast(matrix(0.98, 10, 10))
  terra::crs(dummy_em) <- "EPSG:4326"

  test <- tuav_create(path = "../Test_image/DJI_20240806173534_0039_T.JPG", camera = "DJI_M3T")
  test_correct <- tuav_correct(test)

  result <- tuav_emis(thermal_orig = dummy_raster,
                      thermal_uav = test_correct,
                      corrmap = dummy_em,
                      method = "EM")

  expect_s4_class(result, "SpatRaster")
})












