context("facet manipulation")

test_that("facet creation and addition of entries", {
  f <- Facet()
  e <- FacetEntry()
  expect_s4_class(f, "Facet")
  expect_s4_class(e, "FacetEntry")
  f <- add_entry(f, e)
  expect_equal(f@entries[[1]], e)
})

test_that("named facet creation and addition of entries", {
  f <- NamedFacet()
  e <- NamedFacetEntry(name = "test")
  expect_s4_class(f, "NamedFacet")
  expect_s4_class(e, "NamedFacetEntry")
  f <- add_entry(f, e)
  expect_equal(f@entries[["test"]], e)
})
