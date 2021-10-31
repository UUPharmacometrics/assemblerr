
ParameterComponent <- setClass("ParameterComponent", contains = "NamedComponent", slots = c(name = "character"))
FlowComponent <- setClass("FlowComponent", contains = "Component")

make_test_named_component_list <- function(n) {
  if (n == 0) return(NamedComponentList())
  l <- list()
  for (i in seq_len(n)) {
    l[[i]] <- ParameterComponent(name = paste0("prm_", i))
  }
  return(NamedComponentList(!!!l))
}

test_that("constructor", {
  expect_silent(l <- NamedComponentList(
    ParameterComponent(name = "cl"),
    ParameterComponent(name = "v")
  ))
  expect_error(NamedComponentList(data.frame()))
  expect_error(NamedComponentList(FlowComponent()))
})

test_that("is_empty", {
  l <- make_test_named_component_list(0)
  expect_true(is_empty(l))

  l <- make_test_named_component_list(2)
  expect_false(is_empty(l))
})

test_that("length", {
  l <- make_test_named_component_list(0)
  expect_equal(length(l), 0)

  l <- make_test_named_component_list(2)
  expect_equal(length(l), 2)
})

test_that("append", {
  l <- make_test_named_component_list(2)
  l <- append(l, ParameterComponent(name = "prm3"))
  expect_equal(length(l), 3)
  expect_equal(l[["prm3"]]@name, "prm3")
})


test_that("[[", {
  l <- make_test_named_component_list(2)
  expect_equal(l[[1]]@name, "prm_1")
  expect_equal(l[["prm_2"]]@name, "prm_2")
  expect_error(l[[1,1]])
})


test_that("[[ <- ", {
  l <- make_test_named_component_list(2)
  l[[2]] <- NULL
  expect_equal(length(l), 1)
  l[[2]] <- ParameterComponent(name = "test")
  expect_equal(l[[2]]@name, "test")
  expect_equal(l[["test"]]@name, "test")
})

