context("expression")

test_that("conversion from formula works", {
  expect_equal(as_expr(~a+b), quote(a+b))
  expect_equal(as_expr(cl~theta*exp(eta)), quote(cl <- theta*exp(eta)))
})

test_that("conversion from lists works", {
  l <- list(cl~theta*exp(eta), v~theta[1])
  expect_equal(as_expr(l), list(quote(cl <- theta*exp(eta)), quote(v <- theta[1])))
})
