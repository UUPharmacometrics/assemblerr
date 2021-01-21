context("declaration")

test_that("construction works", {
  expect_silent(declaration(cl~theta + eta, v~theta[1]*exp(eta[2])))
  expect_silent(declaration(theta[i]~a))
  expect_silent(declaration(cl = theta + eta))

  expect_error(declaration(a*b~c))
})


test_that("accessing fields works", {
  d <- declaration(cl~theta + eta, v~theta[1]*exp(eta[2]), ka = theta*exp(eta))
  expect_equal(dcl_id(d),
               list(quote(cl),
                    quote(v),
                    quote(ka))
               )
  expect_equal(dcl_def(d),
               list(
                 quote(theta + eta),
                 quote(theta[1]*exp(eta[2])),
                 quote(theta*exp(eta)))
               )
})

test_that("setting fields works", {
  d <- declaration(a ~ x, b ~ y, c ~ z)
  dcl_id(d) <- list(quote(x), quote(y), quote(z))
  expect_equal(dcl_id_label(d), c("x", "y", "z"))
  dcl_id(d) <- quote(a)
  expect_equal(dcl_id_label(d), c("a", "a", "a"))
})

test_that("listing of variables", {
  d <- declaration(cl~theta + eta, v~theta[1]*exp(eta[2]))
  expect_equal(dcl_vars(d, include_lhs = FALSE, include_indicies = FALSE) %>% as.character(),
               c("theta","eta"))
  expect_equal(dcl_vars(d, include_lhs = TRUE, include_indicies = FALSE) %>% as.character(),
               c("cl","v","theta","eta"))
  expect_equal(dcl_vars(d, include_lhs = TRUE, include_indicies = TRUE) %>% as.character(),
               c("cl","v","theta","eta", "theta[1]", "eta[2]"))
})

test_that("substitution of variables", {
  d <- declaration(cl~theta + eta, v~theta[1]*exp(eta[2]))
  expect_equal(
    dcl_substitute(d, list(theta = quote(a))),
    declaration(cl~ a + eta, v~a[1]*exp(eta[2]))
  )
  tmp <- sym("a")
  expect_equal(
    dcl_substitute(d, list(cl = quote(x)), .include_lhs = TRUE),
    declaration(x~theta + eta, v~theta[1]*exp(eta[2]))
  )
  expect_equal(
    dcl_substitute(d, list(cl = quote(x)), .include_lhs = FALSE),
    d
  )
  expect_equal(
    dcl_substitute(declaration(cl~theta[i]), list(i = 1:2)),
    declaration(cl~theta[1L], cl~theta[2L])
  )
})

test_that("addition of declarations", {
  d1 <- declaration(y1~x1, y2~x2)
  d2 <- declaration(~c1, ~c2)
  expect_equal(dcl_add(d1, d2), declaration(y1~x1+c1, y2~x2+c2))
  expect_equal(dcl_add(d1, declaration(~c)), declaration(y1~x1+c, y2~x2+c))
  expect_equal(dcl_add(d1, declaration(~0)), d1)
})

test_that("substraction of declarations", {
  d1 <- declaration(y1~x1, y2~x2)
  d2 <- declaration(~c1, ~c2)
  expect_equal(dcl_substract(d1, d2), declaration(y1~x1-c1, y2~x2-c2))
  expect_equal(dcl_substract(d1, declaration(~c)), declaration(y1~x1-c, y2~x2-c))
  expect_equal(dcl_substract(d1, declaration(~0)), d1)
  expect_equal(dcl_substract(declaration(y1~0), declaration(~c)), declaration(y1~-c))
})

test_that("multiplication of declarations", {
  d1 <- declaration(y1~x1, y2~x1+x2)
  d2 <- declaration(~c1, ~c2)
  expect_equal(dcl_multiply(d1, d2), declaration(y1~x1*c1, y2~(x1+x2)*c2))
  expect_equal(dcl_multiply(d1, declaration(~c)), declaration(y1~x1*c, y2~(x1+x2)*c))
  expect_equal(dcl_multiply(d1, declaration(~1)), d1)
})


test_that("devision of declarations", {
  d1 <- declaration(y1~x1, y2~x1+x2)
  d2 <- declaration(~c1, ~c2)
  expect_equal(dcl_devide(d1, d2), declaration(y1~x1/c1, y2~(x1+x2)/c2))
  expect_equal(dcl_devide(d1, declaration(~c)), declaration(y1~x1/c, y2~(x1+x2)/c))
  expect_equal(dcl_devide(d1, declaration(~1)), d1)
})

test_that("linearity detection",{
  expect_true(dcl_linear_in(declaration(~k*A), quote(A)))
  expect_false(dcl_linear_in(declaration(~vmax*A/(A+km)), quote(A)))
})

# test_that("combination of declarations", {
#   d1 <- declaration(cl~theta + eta, v~theta[1]*exp(eta[2]))
#   d2 <- declaration(t~b)
#   d3 <- declaration(t1~b, t2~a)
#   expect_equal(dcl_combine(d1, d2, combine_fn = "+"), declaration(cl~theta + eta + b, v~theta[1]*exp(eta[2])+b))
#   expect_equal(dcl_combine(d1, d3, combine_fn = "+"), declaration(cl~theta + eta + b, v~theta[1]*exp(eta[2])+a))
# })


test_that("substitution of array indicies", {
  d <- declaration(cl~theta["test"], v~theta["test"]*eta["test"])
  expect_equal(dcl_substitute_index(d, "theta", c(test = 1)), declaration(cl~theta[1], v~theta[1]*eta["test"]))
})

test_that("", {
  d <- declaration(a ~ b, b ~ c, c ~ d, d ~ e, test ~ c, cl ~ theta, conc ~ A[2], depot ~ A[1])
  expect_equal(dcl_depends_on(d, "e"), c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(dcl_depends_on(d, "A[2]", include_indicies = TRUE),  c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(dcl_depends_on(d, "A", include_indicies = FALSE),  c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE))

})

test_that("external variables are retrieved",{
  d <- declaration(conc~dose/v*exp(-k*time), k~cl/v, cl~theta[1], v~theta[2])
  expect_setequal(dcl_external_variables(d) %>% as.character(), c("theta[1]", "theta[2]", "time", "dose"))
})
