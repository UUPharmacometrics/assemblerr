context("declaration")

test_that("construction works", {
  expect_silent(declaration(cl~theta + eta, v~theta[1]*exp(eta[2])))
  expect_silent(declaration(theta[i]~a))

  expect_error(declaration(a*b~c))
})
