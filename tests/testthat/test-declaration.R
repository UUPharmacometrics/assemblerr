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
