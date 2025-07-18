test_that("tuav_sharp errors on invalid input", {
  fake_input <- list()  # Not a ThermalUAV object
  expect_error(tuav_sharp(fake_input),
               "The provided parameter thermal_uav is not of class ThermalUAV")
})

test_that("tuav_sharp computes sharpness when ThermalData is present", {
  test <- tuav_create(path = "../Test_image/DJI_20240806173534_0039_T.JPG", camera = "DJI_M3T")
  test_correct <- tuav_correct(test)
  result <- tuav_sharp(test_correct)

  expect_s4_class(result, "ThermalUAV")
  expect_length(result@Sharpness@Tsharp, 1)
  expect_type(result@Sharpness@Tsharp, "double")
  expect_true(all(!is.na(result@Sharpness@Tsharp)))
})
