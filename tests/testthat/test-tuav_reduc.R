test_that("tuav_reduc throws error if input is not a ThermalUAV", {
  dummy_input <- list(a = 1, b = 2)
  expect_error(tuav_reduc(dummy_input), "not of class ThermalUAV")
})
