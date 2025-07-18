test_that("tuav_correct throws error for wrong class input", {
  dummy_input <- list()
  expect_error(tuav_correct(dummy_input),
               "The provided parameter thermal_uav is not of class ThermalUAV")
})

test_that("tuav_correct handles minimal valid ThermalUAV object", {
  # Build dummy ThermalUAV object
  test <- tuav_create(path = "../Test_image/DJI_20240806173534_0039_T.JPG", camera = "DJI_M3T")

  # Run function
  result <- tuav_correct(test, T_air = 25, rel_hum = 70, T_bg = NA, SKC = TRUE, emis = 0.985)

  # Check output
  expect_s4_class(result, "ThermalUAV")
  expect_true(!is.null(result@ThermalData))
  expect_equal(dim(result@ThermalData[["DJI_20240806173534_0039_T.JPG"]]), c(512, 640))
  expect_equal(round(result@ThermalData[["DJI_20240806173534_0039_T.JPG"]][1,1],4), 304.7582)
  expect_equal(result@Atmosphere@tuav_correct, "Yes")
})


test_that("tuav_correct throws on bad input for T_air", {
  test <- tuav_create(path = "../Test_image/DJI_20240806173534_0039_T.JPG", camera = "DJI_M3T")

  # Bad input
  bad_T_air <- "not_a_number"

  expect_error(tuav_correct(test, T_air = bad_T_air),
               "T_air is not numeric")
})
