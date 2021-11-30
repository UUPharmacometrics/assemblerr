test_that("template lookup by class", {
  templates <- list(TestClass = "test_template")
  tc <- setClass("TestClass", slots = c(test = "integer"))
  cc <- setClass("ChildClass", contains = "TestClass")
  expect_equal(get_default_template(tc(), template_list = templates), templates[["TestClass"]])
  expect_equal(get_default_template(cc(), template_list = templates), templates[["TestClass"]])
  templates[["ChildClass"]] <- "child_template"
  expect_equal(get_default_template(cc(), template_list = templates), templates[["ChildClass"]])
})

test_that("rendering of objects", {
  tc <- setClass(
    "TestClass",
    slots = c(name = "character", length = "integer"),
    prototype = prototype(name = "test", length = 1L)
  )
  template <- "name: {name}, length: {length}"
  expect_equal(render_component2(tc(), template), "name: test, length: 1")
  template_self <- "name: {self@name}, length: {self@length}"

  expect_equal(render_component2(tc(), template_self), "name: test, length: 1")

})

test_that("rendering of object lists", {
  tc <- setClass(
    "TestClass",
    slots = c(name = "character", length = "integer")
  )
  l <- list(
    tc(name = "a", length = 1L),
    tc(name = "b", length = 2L)
  )
  templates <- list(TestClass = "{name},{length}")
  expect_equal(render_collection(l, collapse = ";", template_list = templates), "a,1;b,2")

})

