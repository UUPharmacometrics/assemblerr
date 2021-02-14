test_that("facet class name to labels", {
  expect_equal(facet_names_to_labels(c("ParameterFacet","InputVariableFacet")), c("parameter", "input variable"))
})

test_that("string interpolation", {
  x <- "test"
  expect_equal(interp("this is a {x}"), "this is a test")
  entries <- 1:3
  expect_equal(interp("{length(entries)} {?entry/entries}: {entries}"), "3 entries: 1, 2, and 3")
})
