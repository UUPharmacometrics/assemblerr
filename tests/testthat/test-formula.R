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

test_that("LHS and RHS can be accessed",{
  expect_equal(fml_get_lhs(c~a+b), quote(c))
  expect_equal(fml_get_rhs(c~a+b), quote(a+b))
})

test_that("LHS and RHS can be set",{
  expect_equal(fml_set_lhs(c~a+b, quote(d)), d~a+b)
  expect_equal(fml_set_rhs(c~a+b, quote(g+d)), quote(c~g+d))
})

test_that("Variables can be listed",{
  expect_setequal(fml_vars(~emax*conc/(conc+ec50)), c("emax","conc","ec50"))
  expect_setequal(fml_vars(~theta[1]*exp(eta[1])), c("theta","eta"))
})

test_that("Functions can be listed",{
  expect_setequal(fml_funs(~emax*conc/(conc+ec50)), c("/","*","(","+"))
  expect_setequal(fml_funs(~theta[1]*exp(eta[1])), c("*","exp", "["))
})

test_that("Formulas can be combined", {
  expect_equal(fml_combine(~a, ~b, '+'), ~a+b)
  expect_equal(fml_combine(~a, ~b+c, '/'), ~a/(b+c))
  expect_equal(fml_combine(~a, ~b+c, '/', quote(test)), test~a/(b+c))
})
