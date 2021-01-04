context("nonmem-integration")

skip_on_cran()
# check if PsN is available
skip_if_not(system("psn -version", intern = FALSE, ignore.stdout = TRUE) == 0, "PsN installation not found.")


test_nm_model_execution <- function(model, options = assemblerr_options()) {
  local_create_nonmem_test_directory()
  create_dummy_data(model, path = "data.csv")
  render(model, "run1.mod", options = options)

  expect_true(file.exists("run1.mod"))
  system("execute run1.mod", intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
  if (interactive() && file.exists("run1.lst")) system("cat run1.lst")
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


test_that("1cmp linear, advan", {
  m <- pk_model() +
    pk_distribution_1cmp() +
    pk_elimination_linear() +
    obs_additive(conc~C["central"])

  test_nm_model_execution(m,
                          assemblerr_options(ode.use_special_advans = TRUE))
})

test_that("1cmp linear, ode", {
  m <- pk_model() +
    pk_distribution_1cmp() +
    pk_elimination_linear() +
    obs_additive(conc~C["central"])

  test_nm_model_execution(m,
                          assemblerr_options(ode.use_special_advans = FALSE,
                                             ode.use_general_linear_advans = FALSE))
})
