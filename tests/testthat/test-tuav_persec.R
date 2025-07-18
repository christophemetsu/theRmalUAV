test_that("tuav_persec errors when input is not ThermalUAV", {
  expect_error(tuav_persec(list()),
               "The provided parameter thermal_uav is not of class ThermalUAV")
})
