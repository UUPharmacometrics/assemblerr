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
  expect_equal(dec_get_def(m$compartments[["volume"]][[1]]), 1)
  expect_equal(m$compartments[["index"]][1], 1)
  expect_equal(m$compartments[["name"]][1], "test")
})

test_that("flows can be added", {
  m <- model() +
    flow("source", "sink", ~k)
  expect_equal(m$flows[["from"]][1], "source")
  expect_equal(m$flows[["to"]][1], "sink")
  expect_equal(dec_get_def(m$flows[["definition"]][[1]]), quote(k))
  expect_equal(m$flows[["index"]][1], 1)
})
