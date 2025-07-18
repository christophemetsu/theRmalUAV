test_that("coreg_prep throws error when img_path is missing", {
  expect_error(coreg_prep(img_path = NA))
})

test_that("coreg_prep throws error for unknown camera", {
  fake_path <- tempdir()
  expect_error(coreg_prep(img_path = fake_path, camera_name = "NotARealCamera"))

})

test_that("coreg_prep throws error for invalid opt_camera_path", {
  fake_path <- tempdir()
  expect_error(coreg_prep(img_path = fake_path, SfM_option = "Agisoft Metashape",
                            opt_camera_path = NA, label = "_2"))

})
