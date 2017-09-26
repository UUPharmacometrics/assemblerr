context("NM code syntax elements")

test_that("code generation from 'static' expressions works", {
  code_expression <- NMExpression$new("CL","=","1")
  expect_equal(code_expression$get_code(), "CL=1")
})

test_that("code generation from expressions with other syntax elements works", {
  theta <- NMTheta$new("CL")
  theta$index <- 1
  code_expression <- NMExpression$new("CL","=",theta)
  expect_equal(code_expression$get_code(), "CL=THETA(1)")
})


context("NM code facet")

test_that("code generation from 'static' expressions works from add_new", {
  code_facet <- NMCodeFacet$new()
  nm_expression <- code_facet$add_new("CL","=","1")
  expect_equal(nm_expression$get_code(), "CL=1")
})

test_that("code generation from expressions with other syntax elements works from add_new", {
  theta <- NMTheta$new("CL")
  theta$index <- 1
  code_facet <- NMCodeFacet$new()
  nm_expression <- code_facet$add_new("CL","=",theta)
  expect_equal(nm_expression$get_code(), "CL=THETA(1)")
})

context("NM parameter facet")
test_that("code generation works from add_new", {
  theta <- NMTheta$new("CL")
  theta$index <- 1
  code_facet <- NMParameterFacet$new()
  nm_expression <- code_facet$add_new(name = "CL", theta)
  expect_equal(nm_expression$get_code(), "CL=THETA(1)")
})
