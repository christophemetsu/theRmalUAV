test_that("tuav_smooth errors for invalid object", {
  expect_error(tuav_smooth("not a tuav"), "not of class ThermalUAV")
})

test_that("tuav_smooth errors if no ThermalData", {
  test <- tuav_create(path = "../Test_image/DJI_20240806173534_0039_T.JPG", camera = "DJI_M3T")
  expect_error(tuav_smooth(test), "ThermalData not found")
})

test_that("tuav_smooth works with method 'image'", {
  test <- tuav_create(path = "../Test_image/", camera = "DJI_M3T")
  test_correct <- tuav_correct(test)
  result <- tuav_smooth(test_correct, method = "image")

  expect_s4_class(result, "ThermalUAV")
  expect_equal(result@Smooth@method, "image")
  expect_equal(length(result@Smooth@T_smooth), test_correct@Info@dataset_length)
})

test_that("tuav_smooth works with method 'T_air' and numeric vector", {
  test <- tuav_create(path = "../Test_image/", camera = "DJI_M3T")
  test_correct <- tuav_correct(test)

  T_air <- rep(24, test_correct@Info@dataset_length)
  result <- tuav_smooth(test_correct, method = "T_air", T_air = T_air, smooth_length = 5)
  expect_s4_class(result, "ThermalUAV")
  expect_equal(result@Smooth@method, "T_air")
})

test_that("tuav_smooth works with method 'T_air' and data.frame input", {
  test <- tuav_create(path = "../Test_image/", camera = "DJI_M3T")
  test_correct <- tuav_correct(test)

  T_air_df <- data.frame(
    datetime = c(test_correct@Info@TTime[1] - 1, test_correct@Info@TTime ,test_correct@Info@TTime[test_correct@Info@dataset_length] + 1),
    T_air = rep(24, test_correct@Info@dataset_length+2)
  )
  result <- tuav_smooth(test_correct, method = "T_air", T_air = T_air_df, smooth_length = 5)
  expect_s4_class(result, "ThermalUAV")
})

test_that("tuav_smooth errors on non-numeric smooth_length", {
  test <- tuav_create(path = "../Test_image/", camera = "DJI_M3T")
  test_correct <- tuav_correct(test)
  expect_error(tuav_smooth(test_correct, method = "image", smooth_length = "a"),
               "smooth_length is not numeric")
})


