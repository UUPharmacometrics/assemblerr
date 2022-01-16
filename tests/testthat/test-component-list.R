TestEntryComponent <- setClass("TestEntryComponent", contains = "Component", slots = c(index = "integer"))

make_test_component_list <- function(n) {
  if (n == 0) return(ComponentList())
  l <- list()
  for (i in seq_len(n)) {
    l[[i]] <- TestEntryComponent(index = i)
  }
  return(ComponentList(!!!l))
}

test_that("contructor", {
  ParameterComponent <- setClass("ParameterComponent", contains = "Component")
  expect_silent(l <- ComponentList(ParameterComponent(), ParameterComponent()))
  expect_error(ComponentList(data.frame()))
})


test_that("is_empty", {
  l <- make_test_component_list(0)
  expect_true(is_empty(l))

  l <- make_test_component_list(2)
  expect_false(is_empty(l))
})

test_that("length", {
  l <- make_test_component_list(0)
  expect_equal(length(l), 0)

  l <- make_test_component_list(2)
  expect_equal(length(l), 2)
})

test_that("append", {
  l <- make_test_component_list(2)
  l <- append(l, TestEntryComponent(index = 3L))
  expect_equal(length(l), 3)
})


test_that("[[", {
  l <- make_test_component_list(2)
  expect_equal(l[[1]]@index, 1)
  expect_error(l[[1,1]])
})


test_that("[[ <- ", {
  l <- make_test_component_list(2)
  l[[2]] <- NULL
  expect_equal(length(l), 1)
  l[[2]] <- TestEntryComponent(index = 0L)
  expect_equal(l[[2]]@index, 0L)
})

test_that("add_component", {
  l <- make_test_component_list(2)
  l <- append(l, TestEntryComponent(index = 3L))
  expect_equal(l[[3]]@index, 3L)
})
