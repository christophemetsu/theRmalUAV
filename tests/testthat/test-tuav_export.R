test_that("tuav_export fails for wrong input type", {
  expect_error(tuav_export(list()),
               "not of class ThermalUAV")
})

test_that("tuav_export fails if ThermalData is empty", {
  test_thermaluav <- tuav_create(path = "../Test_image/DJI_20240806173534_0039_T.JPG", camera = "DJI_M3T")
  expect_error(tuav_export(test_thermaluav),
               "ThermalData is empty")
})

test_that("tuav_export fails if export_path does not exist", {
  test_thermaluav <- tuav_create(path = "../Test_image/DJI_20240806173534_0039_T.JPG", camera = "DJI_M3T")
  test_thermaluav <- tuav_correct(test_thermaluav)
  bad_path <- tempfile() # will not be created
  expect_error(tuav_export(test_thermaluav, export_path = bad_path),
               "does not exist")
})
