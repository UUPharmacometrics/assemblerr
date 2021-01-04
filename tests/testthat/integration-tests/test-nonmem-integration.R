context("nonmem-integration")

skip_on_cran()
# check if PsN is available
skip_if_not(system("psn -version", intern = FALSE, ignore.stdout = TRUE) == 0)


test_nm_model_execution <- function(model) {
  local_create_nonmem_test_directory()
  create_dummy_data(model, path = "data.csv")
  render(model, "run1.mod")

  expect_true(file.exists("run1.mod"))
  system("execute run1.mod", intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
  expect_true(file.exists("run1.lst"))
}

library(assemblerr)

test_that("simple pred model", {
  m <- model() +
    prm_log_normal("v") +
    prm_log_normal("cl") +
    obs_additive(conc~amt/v*exp(-cl/v*time))

  test_nm_model_execution(m)
})
