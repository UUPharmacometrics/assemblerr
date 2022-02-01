local_edition(3)

test_that("prm_log_normal", {
  nm <- NmModel2()
  m <- model() + prm_log_normal("test")

  nm <- convert(nm, m, m@facets$ParameterFacet@entries$test, assemblerr_options())
  expect_named(nm@thetas, "test")
  expect_named(nm@omegas, "test")
  expect_snapshot_output(as.character(nm@pk@statements))
})


test_that("prm_normal", {
  nm <- NmModel2()
  m <- model() + prm_normal("test")

  nm <- convert(nm, m, m@facets$ParameterFacet@entries$test, assemblerr_options())
  expect_named(nm@thetas, "test")
  expect_named(nm@omegas, "test")
  expect_snapshot_output(as.character(nm@pk@statements))
})

test_that("prm_logit_normal", {
  nm <- NmModel2()
  m <- model() + prm_logit_normal("test")

  nm <- convert(nm, m, m@facets$ParameterFacet@entries$test, assemblerr_options())
  expect_named(nm@thetas, "test")
  expect_named(nm@omegas, "test")
  expect_snapshot_output(as.character(nm@pk@statements))
})

test_that("prm_no_var", {
  nm <- NmModel2()
  m <- model() + prm_no_var("test")

  nm <- convert(nm, m, m@facets$ParameterFacet@entries$test, assemblerr_options())
  expect_named(nm@thetas, "test")
  expect_snapshot_output(as.character(nm@pk@statements))
})
