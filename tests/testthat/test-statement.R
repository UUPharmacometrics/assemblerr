context("test-statement")

test_that("creation", {
  s <- statement(cl <- theta[1]*exp(eta[1]), v <- theta[2])
  expect_s3_class(s, "assemblerr_statement")
  expect_length(s, 2)
})

test_that("creation with quoting works", {
  a <- 10
  s <- statement(cl <- theta[!!a]*exp(eta[!!a]))
  expect_equal(s, statement(cl <- theta[10]*exp(eta[10])))
})


test_that("conversion from declaration",{
  d <- declaration(cl~theta[1]*exp(eta[1]), v ~ theta[2], ~exp(eta[1]))
  s <- as_statement(d)
  expect_equal(s[1], statement(cl <- theta[1]*exp(eta[1])))
  expect_equal(s[2], statement(v <- theta[2]))
  expect_equal(s[3], statement(exp(eta[1])))
})

test_that("code rendering", {
  s <- statement(
    cl <- theta[1]*exp(eta[1]),
    v <- theta[2]
  )
  expect_equal(as_code(s),
               "CL = THETA(1) * EXP(ETA(1))\nV = THETA(2)")

})
