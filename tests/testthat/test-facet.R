context("Facets")

test_that("adding to facets works", {
  facet <- Facet$new()
  facet$add(name = "compartments", item = 10)
  expect_equal(facet$count(), 1)
})

test_that("add_new works without constructor arguments", {
  theta_facet <- NMThetaFacet$new()
  theta_facet$add_new(name = "test")
  expect_equal(theta_facet$count(), 1)
  expect_is(theta_facet$get("test"), "NMTheta")
})

test_that("add_new works with constructor arguments", {
  theta_facet <- NMThetaFacet$new()
  theta <- theta_facet$add_new(name = "test", lbound = 0)
  expect_equal(theta_facet$count(), 1)
  expect_is(theta, "NMTheta")
  expect_equal(theta$lbound, 0)
})



test_that("retrival from facets works",{
  facet <- Facet$new()
  facet$add(name = "compartments", item = 10)
  expect_equal(facet$get("compartments"), 10)
})


test_that("type constraints work", {
  theta_facet <- NMThetaFacet$new()
  expect_error(theta_facet$add("test", 1))
})


