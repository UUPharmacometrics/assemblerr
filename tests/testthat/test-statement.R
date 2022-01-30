context("test-statement")

test_that("creation", {
  s <- statement(quote(cl <- theta[1]*exp(eta[1])),
                 quote(v <- theta[2]))
  expect_s3_class(s, "assemblerr_statement")
  expect_length(s, 2)
})

test_that("creation from character", {
  expect_equal(
    statement("cl <- theta"),
    statement(quote(cl <- theta))
  )
  index <- 5
  expect_equal(
    statement("cl <- theta[{index}]"),
    statement(quote(cl <- theta[5]))
  )
})

test_that("creation with quoting works", {
  a <- 10
  s <- statement(bquote(cl <- theta[.(a)]*exp(eta[.(a)])))
  expect_equal(s, statement(quote(cl <- theta[10]*exp(eta[10]))))
})


test_that("conversion from declaration",{
  d <- declaration(cl~theta[1]*exp(eta[1]), v ~ theta[2], ~exp(eta[1]))
  s <- as_statement(d)
  expect_equal(s[1], statement(quote(cl <- theta[1]*exp(eta[1]))))
  expect_equal(s[2], statement(quote(v <- theta[2])))
  expect_equal(s[3], statement(quote(exp(eta[1]))))
})

test_that("code rendering", {
  s <- statement(
    quote(cl <- theta[1]*exp(eta[1])),
    quote(v <- theta[2])
  )
  expect_equal(render_component(s),
               "CL = THETA(1) * EXP(ETA(1))\nV = THETA(2)")

})

test_that("conversion to character", {
  s <- statement(quote(v <- theta[2]))
  expect_equal(as.character(s), "v <- theta[2]")
})
