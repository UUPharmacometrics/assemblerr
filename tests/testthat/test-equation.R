context("Equations")

expected <- list(
  numeric = structure(list(lhs = NULL, rhs = 1), class = "equation"),
  one_sided = structure(list(lhs = NULL, rhs = quote(ka*A)), class = "equation"),
  two_sided = structure(list(lhs = quote(y), rhs = quote(ka*A)), class = "equation")
)

test_that("Equations are created correctly", {
  # creation from expression
  expect_equal(equation(ka*A), expected$one_sided)

  # creation from formula
  expect_equal(equation(~ka*A), expected$one_sided)

  # creation of two sided equation
  expect_equal(equation(y~ka*A), expected$two_sided)
})

test_that("Conversion of one-sided equations works", {
  # Formula
  expect_equal(as_equation(~ka*A), expected$one_sided)
  expect_equal(as_equation(y~ka*A), expected$two_sided)

  # character
  expect_equal(as_equation("ka*A"), expected$one_sided)

  # numeric
  expect_equal(as_equation(1), expected$numeric)
})
