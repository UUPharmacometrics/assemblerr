test_that("adding  NamedNodes to NodeList", {
  nl <- NodeList() + NamedNode(name = "test")
  expect_is(nl, "NodeList")
  expect_length(nl, 1)
  expect_equal(nl[[1]]@name, "test")
})

test_that("adding  NamedNodes to NamedNodeList", {
  nl <- NamedNodeList() + NamedNode(name = "test")
  expect_is(nl, "NodeList")
  expect_length(nl, 1)
  expect_equal(nl[[1]]@name, "test")
  expect_equal(nl[["test"]]@name, "test")
})

test_that("combining NodeLists", {
  nl0 <- NodeList() + NodeList()
  expect_is(nl0, "NodeList")
  expect_length(nl0, 0)
  nl2 <- NodeList() + NamedNode(name = "test") + {NodeList() + NamedNode(name = "test2")}
  expect_is(nl2, "NodeList")
  expect_length(nl2, 2)
  expect_equal(nl2[[1]]@name, "test")
  expect_equal(nl2[[2]]@name, "test2")
})


test_that("combining NamedNodeLists", {
  nl0 <- NamedNodeList() + NamedNodeList()
  expect_is(nl0, "NamedNodeList")
  expect_length(nl0, 0)
  nl2 <- NamedNodeList() + NamedNode(name = "test") + {NamedNodeList() + NamedNode(name = "test2")}
  expect_is(nl2, "NamedNodeList")
  expect_length(nl2, 2)
  expect_equal(nl2[[1]]@name, "test")
  expect_equal(nl2[[2]]@name, "test2")
  expect_equal(nl2[["test"]]@name, "test")
  expect_equal(nl2[["test2"]]@name, "test2")
})
