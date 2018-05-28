context("rendering declaration")

test_that("unallowed functions are reported", {
  # all functions are allowed if no list is given
  expect_true(test_allowed_funs(~exp(a)))
  # true when all function are allowed
  expect_true(test_allowed_funs(~exp(a)+sin(b), c("exp", "+", "sin")))
  # error when function is missing
  expect_error(test_allowed_funs(~exp(a)+sin(b), c("exp", "sin")))
})

