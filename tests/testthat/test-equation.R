context("Equations creation")

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

test_that("Conversion of to equations works", {
  # Formula
  expect_equal(as_equation(~ka*A), expected$one_sided)
  expect_equal(as_equation(y~ka*A), expected$two_sided)

  # character
  expect_equal(as_equation("ka*A"), expected$one_sided)

  # numeric
  expect_equal(as_equation(1), expected$numeric)
})

context("Equation querying")

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

test_that("Function names are listed correctly", {
  # simple non-linear
  expect_equal(equation(cl~theta*exp(eta)) %>% functions(), c("*", "exp"))

})


context("Equation manipulation")

test_that("RHS can be set",{
  equation(cl) %>%
    set_rhs(ka*A) %>%
    expect_equal(expected$one_sided)
})

test_that("LHS can be set",{
  equation(ka*A) %>%
    set_lhs(y) %>%
    expect_equal(expected$two_sided)
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

test_that("Combination through addition works",{
  a <- equation(A)
  b <- equation(B)
  expect_equal(a+b, equation(A+B))
})

test_that("Combination through substraction works",{
  a <- equation(A)
  b <- equation(B)
  expect_equal(a-b, equation(A-B))
})

test_that("Replacing a specific function works",{
  addition_transformer <- function(node, new_op){
    if(rlang::is_lang(node) && rlang::lang_name(node) == "+"){
      node[[1]] <- new_op
    }
    node
  }
  transform_ast(quote(A+B), addition_transformer, new_op = quote(`-`)) %>%
    expect_equal(quote(A-B))
})
