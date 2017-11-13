context("Equations")

test_that("Formula can be converted to an equation", {
  eqn <- as_equation(~cl)
  expect_s3_class(eqn, "equation")
})
