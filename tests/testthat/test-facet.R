context("facet manipulation")

test_that("facets added to a list show have columns index and name",{
  m <- list()
  m <- add_facet(model = m, facet = "parameters")
  expect_true(exists("name", m$parameters))
  expect_true(exists("index", m$parameters))

  expect_equal(typeof(m$parameters$name), "character")
  expect_equal(typeof(m$parameters$index), "integer")
})


test_that("facets have the specified properties and types",{
  m <- list()
  m <- add_facet(model = m, facet = "parameters",
                 property_def = list(value = numeric(),
                                     type = character(),
                                     options = list()))

  expect_true(exists("value", m$parameters))
  expect_true(exists("type", m$parameters))
  expect_true(exists("options", m$parameters))

  expect_equal(typeof(m$parameters$value), "double")
  expect_equal(typeof(m$parameters$type), "character")
  expect_equal(typeof(m$parameters$options), "list")
})

test_that("name column can be dropped", {
  m <- list()
  m <- add_facet(model = m, facet = "parameters", name_column = F)
  expect_false(exists("name", m$parameters))
  expect_true(exists("index", m$parameters))
})

test_that("querying for facets works", {
  m <- list() %>% add_facet("parameters")
  expect_true(has_facet(m, "parameters"))
  expect_false(has_facet(m, "values"))
})

context("fragment manipulation")
test_that("non-existing facets from f2 are added to f1", {
    f1 <- list() %>% add_facet("f1facet1")
    f2 <- list() %>% add_facet("f2facet1")

    f3 <- add_fragment(f1, f2)
    expect_true(exists("f1facet1", f3))
    expect_true(exists("f2facet1", f3))
})

test_that("facets that do not have a name column are appended to the corresponding f1 facet", {
  f1 <- list(
    facet1 = tibble::tibble(index = 1, type = "a")
  )
  f2 <- list(
    facet1 = tibble::tibble(index = 1, type = "b")
  )
  f3 <- add_fragment(f1, f2)
  expect_equal(f3$facet1$index, c(1,1))
  expect_equal(f3$facet1$type, c("a","b"))
})
