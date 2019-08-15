context("formula")

test_that("LHS is validated correctly", {
  expect_true(fml_has_valid_lhs(~a))
  expect_true(fml_has_valid_lhs(test~a))
  expect_true(fml_has_valid_lhs(test[1]~a))

  expect_false(fml_has_valid_lhs(~a, allow_null = FALSE))
  expect_false(fml_has_valid_lhs(a+b~a))
})
