context("declaration")

test_that("construction works", {
  expect_silent(declaration(cl~theta + eta, v~theta[1]*exp(eta[2])))
  expect_silent(declaration(theta[i]~a))

  expect_error(declaration(a*b~c))
})


test_that("accessing fields works", {
  d <- declaration(cl~theta + eta, v~theta[1]*exp(eta[2]))
  expect_equal(dcl_id(d),
               list(quote(cl),
                    quote(v))
               )
  expect_equal(dcl_def(d),
               list(
                 quote(theta + eta),
                 quote(theta[1]*exp(eta[2])))
               )
})

test_that("listing of variables", {
  d <- declaration(cl~theta + eta, v~theta[1]*exp(eta[2]))
  expect_equal(dcl_vars(d, include_lhs = FALSE, include_indicies = FALSE) %>% as.character(),
               c("theta","eta"))
  expect_equal(dcl_vars(d, include_lhs = TRUE, include_indicies = FALSE) %>% as.character(),
               c("cl","v","theta","eta"))
  expect_equal(dcl_vars(d, include_lhs = TRUE, include_indicies = TRUE) %>% as.character(),
               c("cl","v","theta","eta", "theta[1]", "eta[2]"))
})
