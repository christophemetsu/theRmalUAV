test_that("tuav_sharp_thresh errors on wrong input", {
  fake_input <- list()
  expect_error(tuav_sharp_thresh(fake_input),
               "The provided parameter thermal_uav is not of class ThermalUAV")
})
