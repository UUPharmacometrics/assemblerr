context("Model manipulation")

test_that("Model is created correctly", {
  m <- model()
  expect_setequal(list_facets(m),
                  c("compartments", "flows", "parameters",
                    "algebraics", "observations", "meta_tags"))
})

test_that("compartments can be added", {
  m <- model() +
    compartment("test")
  expect_equal(m$compartments$volume, declaration(~1))
  expect_equal(m$compartments$index, 1)
  expect_equal(m$compartments$name, "test")
})

test_that("flows can be added", {
  m <- model() +
    flow("source", "sink", declaration(~k))
  expect_equal(m$flows$from, "source")
  expect_equal(m$flows$to, "sink")
  expect_equal(m$flows$definition, declaration(~k))
  expect_equal(m$flows$index, 1)
})


test_that("algebraics can be added", {
  m <- model() +
    algebraic(declaration(effect~emax*conc/(conc+ec50)))
  expect_equal(m$algebraics$definition, declaration(effect~emax*conc/(conc+ec50)))
  expect_equal(m$algebraics$name, "effect")
  expect_equal(m$algebraics$index, 1)
})

test_that("meta_tags can be added", {
  m <- model() +
    meta_tag("name", "test")
  expect_equal(m$meta_tags$value, "test")
  expect_equal(m$meta_tags$name, "name")
  expect_equal(m$meta_tags$index, 1)
})

