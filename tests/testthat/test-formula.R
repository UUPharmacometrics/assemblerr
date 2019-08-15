context("Formula")

test_that("LHS is validated correctly", {
  expect_true(fml_has_valid_lhs(~a))
  expect_true(fml_has_valid_lhs(test~a))
  expect_true(fml_has_valid_lhs(test[1]~a))

  expect_false(fml_has_valid_lhs(~a, allow_null = FALSE))
  expect_false(fml_has_valid_lhs(a+b~a))
})

test_that("anonymous formulas are identified correctly", {
  expect_true(fml_is_anonymous(~a+b))
  expect_false(fml_is_anonymous(c~a+b))
})
