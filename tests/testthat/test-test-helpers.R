test_that("creation of dummy data", {
  dir <- local_create_nonmem_test_directory()
  m <- model() +
    prm_log_normal("v") +
    prm_log_normal("cl") +
    obs_additive(conc~amt/v*exp(-cl/v*time))
  create_dummy_data(m, path = "data.csv")
  expect_true(file.exists(file.path(dir, "data.csv")))
})
