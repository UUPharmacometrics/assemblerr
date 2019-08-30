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
  expect_setequal(fml_vars(~theta[1]*exp(eta[1]) + a*b, include_indicies = TRUE), c("theta[1]","eta[1]", "a", "b"))
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

test_that("Indicies can be substituted",{
  fml <- dadt["central"] ~ ka*A["central"]
  expect_equal(fml_subs_idx(fml, "dadt", list(central = 1)), dadt[1]~ka*A["central"])
  expect_equal(fml_subs_idx(fml, "A", list(central = 'test')), dadt["central"]~ka*A["test"])
  expect_warning(fml_subs_idx(fml, "A", list()))
})

test_that("Symbols can be substituted",{
  fml <- dadt["central"] ~ ka*A["central"]
  expect_equal(fml_subs_sym(fml, ka = quote(cl), A = quote(B)), dadt["central"]~cl*B["central"])
})

test_that("dependencies are recognized correctly", {
  fmls <- list(cl~theta1*exp(eta2), v~theta2, k~cl/v, dadt[1]~k*A[1], dadt[2]~ka*A[2])
  expect_true(fml_depends_on("k","theta1", fmls))
  expect_true(fml_depends_on("dadt[1]","theta1", fmls))
  expect_true(fml_depends_on("dadt[1]","A[1]", fmls))
  expect_true(fml_depends_on("dadt[1]","A", fmls, include_indicies = FALSE))

  expect_false(fml_depends_on("dadt[1]","ka", fmls))
  expect_false(fml_depends_on("dadt[1]","A[2]", fmls))
  expect_false(fml_depends_on("v","theta1", fmls))
})

test_that("direct dependants are recognized", {
  fmls <- list(cl~theta1*exp(eta2), v~theta2, k~cl/v, dadt[1]~k*A[1], dadt[2]~ka*A[2])
  expect_equal(fmls_direct_dependants(fmls, "cl"), c(3))
  expect_equal(fmls_direct_dependants(fmls, "A"), c(4,5))
})

test_that("topologic order works",{
  fmls <- list(dadt[1]~k*A[1], cl~theta1*exp(eta2), v~theta2, k~cl/v)
  expect_equal(fmls_topologic_order(fmls), c(2,3,4,1))
})
