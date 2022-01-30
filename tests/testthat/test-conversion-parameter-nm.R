test_that("prm_log_normal", {
  nm <- NmModel2()
  m <- model() + prm_log_normal("test")
  o <- list(prm.use_mu_referencing = FALSE)

  nm <- convert(nm, m, m@facets$ParameterFacet@entries$test, o)
  expect_named(nm@thetas, "test")
  expect_named(nm@omegas, "test")
  expect_match(as.character(nm@pk@statements), "test <- theta\\[\\d\\] \\* exp\\(eta\\[\\d\\]\\)")
})


test_that("prm_normal", {
  nm <- NmModel2()
  m <- model() + prm_normal("test")
  o <- list(prm.use_mu_referencing = FALSE)

  nm <- convert(nm, m, m@facets$ParameterFacet@entries$test, o)
  expect_named(nm@thetas, "test")
  expect_named(nm@omegas, "test")
  expect_match(as.character(nm@pk@statements), "test <- theta\\[\\d\\] \\+ eta\\[\\d\\]")
})
