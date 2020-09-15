context("facet manipulation")

test_that("facet creation", {
  f <- facet("parameter", name = character(), type = character())
  expect_s3_class(f, "assemblerr_facet")
  expect_named(f, c("name", "type", "index"))
  expect_equal(facet_name(f), "parameter")
  expect_type(f$name, "character")
  expect_type(f$type, "character")
  expect_true(facet_has_id_columns(f))
  expect_equal(facet_id_columns(f), "name")
})

test_that("fragment creation", {
  frag <- fragment(parameter = list(name = character(), type = character()))
  expect_s3_class(frag, "assemblerr_fragment")
  expect_true(is_fragment(frag))
  expect_true(has_facet(frag, "parameter"))
})

test_that("fragment addition different facets", {
  frgmt1 <- fragment(parameters = list(name = "cl", type = "normal"))
  frgmt2 <- fragment(observations = list(name = "concentration", type = "additive"))
  frgmtr <- add_fragments(frgmt1, frgmt2)
  expect_equal(list_facets(frgmtr), c("parameters", "observations"))
})

test_that("fragment addition no id_column", {
  frgmt1 <- fragment(test = list(value = "a"))
  frgmt2 <- fragment(test = list(value = "b"))
  frgmtr <- add_fragments(frgmt1, frgmt2)
  expect_equal(frgmtr$test$value, c("a", "b"))
  expect_equal(frgmtr$test$index, c(1, 2))
})

test_that("fragment addition with id_column distinct ids", {
  frgmt1 <- fragment(parameters = list(name = "cl", type = "normal"))
  frgmt2 <- fragment(parameters = list(name = "v", type = "normal"))
  frgmtr <- add_fragments(frgmt1, frgmt2)
  expect_equal(frgmtr$parameters$name, c("cl", "v"))
  expect_equal(frgmtr$parameters$index, c(1, 2))
})

test_that("fragment addition with id_column same ids", {
  frgmt1 <- fragment(parameters = list(name = c("cl", "v"), type = "normal"))
  frgmt2 <- fragment(parameters = list(name = "cl", type = "log-normal"))
  frgmtr <- add_fragments(frgmt1, frgmt2)
  expect_equal(frgmtr$parameters$name, c("v", "cl"))
  expect_equal(frgmtr$parameters$type, c("normal", "log-normal"))
  expect_equal(frgmtr$parameters$index, c(1, 2))
})

test_that("fragment addition with `+` operator", {
  frgmt1 <- fragment(parameters = list(name = c("cl", "v"), type = "normal"))
  frgmt2 <- fragment(parameters = list(name = "cl", type = "log-normal"))
  frgmtr <- frgmt1 + frgmt2
  expect_equal(frgmtr$parameters$name, c("v", "cl"))
  expect_equal(frgmtr$parameters$type, c("normal", "log-normal"))
  expect_equal(frgmtr$parameters$index, c(1, 2))
})

test_that("lookup of properties by name", {
  frgmt1 <- fragment(parameters = list(name = c("cl", "v"), type = "normal"),
                     compartments = list(name = c("central"), volume = declaration(~Vc)))
  property <- get_by_name(frgmt1, "parameters", "cl")
  expect_equal(property$name, "cl")
  expect_equal(property$type, "normal")
  expect_equal(property$index, 1)
})
