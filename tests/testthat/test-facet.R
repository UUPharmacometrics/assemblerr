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

test_that("fragments can be created through item()", {
  frgm <- item("test", name = "test", args = list(a = 1, b = 2))
  expect_is(frgm, "fragment")
  expect_equal(frgm$test$name, "test")
  expect_equal(frgm$test$args[[1]], list(a = 1, b = 2))
})

test_that("properties can be retrieved by name",{
  m <- list()
  m <- add_facet(model = m, facet = "parameters", property_def = list(type = "character", equation = list()))
  frgm <- item("parameters", name = "cl", type = "normal", equation = cl~theta+eta)
  m <- m + frgm
  expect_equal(get_by_name(m, "parameters", "cl"),
               list(type = "normal",  equation = cl~theta+eta, index = 1, name = "cl"))
})

test_that("properties can be retrieved through filtering",{
  m <- list()
  m <- add_facet(model = m, facet = "parameter_values",
                 property_def = list(parameter = character(), value = double()),
                 name_column = FALSE)

  frgm1 <- item("parameter_values", parameter = "cl", value = 1)
  frgm2 <- item("parameter_values", parameter = "cl", value = 2)
  frgm3 <- item("parameter_values", parameter = "v", value = 3)

  m <- m + frgm1 + frgm2 + frgm3
  pvs <- get_all(m, "parameter_values", parameter == "cl") %>%
    purrr::map_dbl("value")
  expect_equal(pvs,
               c(1,2))

})
