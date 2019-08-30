context("expression")

test_that("conversion from formula works", {
  expect_equal(as_expr(~a+b), quote(a+b))
  expect_equal(as_expr(cl~theta*exp(eta)), quote(cl <- theta*exp(eta)))
})
