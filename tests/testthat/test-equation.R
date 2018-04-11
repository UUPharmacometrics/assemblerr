context("Equations")

expected <- list(
  numeric = structure(list(lhs = NULL, rhs = 1), class = "equation"),
  one_sided = structure(list(lhs = NULL, rhs = quote(ka*A)), class = "equation"),
  two_sided = structure(list(lhs = quote(y), rhs = quote(ka*A)), class = "equation"),
  two_sided_with_array = structure(list(lhs = quote(dAdt), rhs = quote(ke*A["central"])), class = "equation")
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


test_that("Types are recognized as equation-like", {
  # Formula
  expect_true(is_equationish(~ka*A))
  expect_true(is_equationish(y~ka*A))

  # numeric
  expect_true(is_equationish(1))

  # equation
  expect_true(is_equationish(expected$one_sided))
  expect_true(is_equationish(expected$two_sided))

})

test_that("Variable names are listed correctly", {
  # simple non-linear
  expect_equal(equation(cl~theta*exp(eta)) %>% variables(), c("theta", "eta"))

  # array variables
  expect_equal(equation(cl~theta[1]*exp(eta[2])) %>% variables(), c("theta", "eta"))
})

test_that("Variables can be substituted with symbols", {
  equation(a~b*c) %>%
    substitute(a = rlang::sym("y"), b = rlang::sym("ka"), c = rlang::sym("A")) %>%
    expect_equal(expected$two_sided)

  # array variable
  equation(dAdt~ke*C["central"]) %>%
    substitute(C = quote(A)) %>%
    expect_equal(expected$two_sided_with_array)
})

test_that("Variables can be substituted with expressions", {
  equation(y~a) %>%
    substitute(a = quote(ka*A)) %>%
    expect_equal(expected$two_sided)
})

test_that("Array indicies can be substituted", {
  equation(dAdt~ke*A["test"]) %>%
    substitute_indicies("A", list(test = "central")) %>%
    expect_equal(expected$two_sided_with_array)
})
