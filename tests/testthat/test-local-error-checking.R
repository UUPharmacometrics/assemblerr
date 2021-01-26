
# compartment -------------------------------------------------------------


test_that("compartment", {
  expect_error(compartment())
  expect_error(compartment(1))
  expect_error(compartment("test cmp"))
  expect_silent(compartment("test", 1))
  expect_silent(compartment("test", ~vc))
})


# flow --------------------------------------------------------------------


test_that("flow", {
  expect_error(flow(~ka))
  expect_error(flow(~ka*A, to = "depot"))
  expect_error(flow(~ka*C, to = "depot"))
  expect_silent(flow(~ka*C, from = "depot"))
  expect_silent(flow(~ka, to = "central"))
})



# parameter ---------------------------------------------------------------

test_that("parameter",{
  prm_funs <- lsf.str("package:assemblerr", pattern = "^prm_")
  purrr::walk(prm_funs, ~expect_error(purrr::invoke(.x, name = 1)))
  purrr::walk(prm_funs, ~expect_error(purrr::invoke(.x, name = "_test")))
  purrr::walk(prm_funs, ~expect_error(purrr::invoke(.x, name = "1a")))
  purrr::walk(prm_funs, ~expect_silent(purrr::invoke(.x, name = "test123")))

})

# observation -------------------------------------------------------------

test_that("observation",{
  obs_funs <- lsf.str("package:assemblerr", pattern = "^obs_")

  # declaration
  purrr::walk(obs_funs, ~expect_error(purrr::invoke(.x, prediction = "a+a")))
  purrr::walk(obs_funs, ~expect_error(purrr::invoke(.x, prediction = quote(a+a))))
  purrr::walk(obs_funs, ~expect_silent(purrr::invoke(.x, prediction = "test123")))
  purrr::walk(obs_funs, ~expect_silent(purrr::invoke(.x, prediction = ~test)))
  purrr::walk(obs_funs, ~expect_silent(purrr::invoke(.x, prediction = ~C["central"])))

  # name
  purrr::walk(obs_funs, ~expect_error(purrr::invoke(.x, prediction = ~test, name = "_test")))
  purrr::walk(obs_funs, ~expect_silent(purrr::invoke(.x, prediction = ~test, name = "conc")))

})
