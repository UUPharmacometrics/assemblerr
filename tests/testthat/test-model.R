context("Model manipulation")

test_that("Model is created correctly", {
  m <- model()
  expect_setequal(list_facets(m),
                  c("compartments", "flows", "parameters",
                    "algebraics", "observations",  "parameter_values", "meta_tags"))
})

test_that("compartments can be added", {
  m <- model() +
    compartment("test")
  expect_equal(fml_get_rhs(m$compartments[["volume"]][[1]]), 1)
  expect_equal(m$compartments[["index"]][1], 1)
  expect_equal(m$compartments[["name"]][1], "test")
})

test_that("flows can be added", {
  m <- model() +
    flow("source", "sink", ~k)
  expect_equal(m$flows[["from"]][1], "source")
  expect_equal(m$flows[["to"]][1], "sink")
  expect_equal(fml_get_rhs(m$flows[["definition"]][[1]]), quote(k))
  expect_equal(m$flows[["index"]][1], 1)
})


test_that("algebraics can be added", {
  m <- model() +
    algebraic(effect~emax*conc/(conc+ec50))
  expect_equal(fml_get_rhs(m$algebraics[["definition"]][[1]]), quote(emax*conc/(conc+ec50)))
  expect_equal(fml_get_lhs(m$algebraics[["definition"]][[1]]), quote(effect))
  expect_equal(m$algebraics[["name"]][1], "effect")
  expect_equal(m$algebraics[["index"]][1], 1)
})

test_that("meta_tags can be added", {
  m <- model() +
    meta_tag("name", "test")
  expect_equal(m$meta_tags[["value"]], "test")
  expect_equal(m$meta_tags[["name"]], "name")
  expect_equal(m$meta_tags[["index"]], 1)
})

