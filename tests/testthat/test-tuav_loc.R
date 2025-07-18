test_that("tuav_loc throws error with wrong class", {
  expect_error(tuav_loc(list(a = 1)),
               regexp = "not of class ThermalUAV")
})

test_that("tuav_loc works with basic GPS mode", {
  test_thermaluav <- tuav_create(path = "../Test_image/DJI_20240806173534_0039_T.JPG", camera = "DJI_M3T")
  result <- tuav_loc(test_thermaluav)
  expect_s4_class(result, "ThermalUAV")
  expect_true(!is.null(result@Position@locations_vector))
})

test_that("tuav_loc computes extents when extent = TRUE", {
  test_thermaluav <- tuav_create(path = "../Test_image/DJI_20240806173534_0039_T.JPG", camera = "DJI_M3T")
  result <- tuav_loc(test_thermaluav, extent = TRUE)
  expect_s4_class(result@Position@extents_vector, "PackedSpatVector")
})

test_that("tuav_loc computes overlap when overlap = TRUE and extent = TRUE", {
  test_thermaluav <- tuav_create(path = "../Test_image/", camera = "DJI_M3T")
  result <- tuav_loc(test_thermaluav, extent = TRUE, overlap = TRUE)
  expect_true(!is.null(result@Position@overlap))
  expect_type(result@Position@overlap, "double")
})
