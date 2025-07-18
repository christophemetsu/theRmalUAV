test_that("tuav_create works for 1 image", {
  test <- tuav_create(path = "../Test_image/DJI_20240806173534_0039_T.JPG", camera = "DJI_M3T")
  expect_s4_class(test, "ThermalUAV")
  expect_true((test@Info@dataset_length == 1))
})

test_that("tuav_create works for folder with 32 images", {
  test <- tuav_create(path = "../Test_image/", camera = "DJI_M3T")
  expect_s4_class(test, "ThermalUAV")
  expect_true((test@Info@dataset_length == 32))
})
