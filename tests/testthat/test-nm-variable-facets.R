context("NM variable facet")
test_that("automatic index generation works", {
  theta_facet <- NMThetaFacet$new()
  theta <- theta_facet$add_new(name = "test")
  expect_equal(theta$index, 1)
  theta2 <- theta_facet$add_new(name = "test2")
  expect_equal(theta2$index, 2)
})


context("NM THETA facets")
test_that("adding an unnamed theta results in an error", {
  theta_facet <- NMThetaFacet$new()
  expect_error(theta_facet$add_new())
})

context("NM THETA syntax element")
test_that("code generation for THETA elements works", {
  theta <- NMTheta$new("Clearance")
  theta$index <- 1
  expect_equal(theta$get_code(), "THETA(1)")
})


context("NM ETA syntax element")
test_that("code generation for ETA elements works", {
  eta <- NMEta$new("Clearance")
  eta$index <- 1
  expect_equal(eta$get_code(), "ETA(1)")
})
