context("declaration creation/conversion")

expected <- list(
  numeric = structure(list(identifier = NULL, definition = 1), class = "declaration"),
  one_sided = structure(list(identifier = NULL, definition = quote(ka*A)), class = "declaration"),
  two_sided = structure(list(identifier = quote(y), definition = quote(ka*A)), class = "declaration"),
  two_sided_with_array = structure(list(identifier = quote(dAdt), definition = quote(ke*A["central"])), class = "declaration")
)

test_that("declarations can be created",{
  # anonymous
  expect_equal(declaration(definition = ka*A), expected$one_sided)
  # named
  expect_equal(declaration("y", ka*A), expected$two_sided)
})


test_that("formula can be converted to declarations", {
  # anonymous
  expect_equal(as_declaration(~ka*A), expected$one_sided)
  # named
  expect_equal(as_declaration(y~ka*A), expected$two_sided)
})

test_that("conversion fails for formula with functions on LHS",{
  expect_error(as_declaration(y+x~ka*A))
  expect_error(as_declaration(log(y)~ka*A))
})

test_that("characters can be converted to declarations", {
  # anonymous
  expect_equal(as_declaration("ka*A"), expected$one_sided)
  expect_equal(as_declaration("~ka*A"), expected$one_sided)
  # named
  expect_equal(as_declaration("y~ka*A"), expected$two_sided)
})

test_that("conversion fails for invalid syntax characters",{
  expect_error(as_declaration("ka*"))
})

test_that("numbers can be converted to declarations", {
  expect_equal(as_declaration(1), expected$numeric)
})

context("declaration querying")

test_that("types are recognized as equation-like", {
  # Formula
  expect_true(is_declarationish(~ka*A))
  expect_true(is_declarationish(y~ka*A))

  # numeric
  expect_true(is_declarationish(1))

  # character
  expect_true(is_declarationish("ka*A", parse = T))

  # declaration
  expect_true(is_declarationish(expected$one_sided))
  expect_true(is_declarationish(expected$two_sided))

})

test_that("syntax is checked when requested", {
  # no parsing --> error is not caught
  expect_true(is_declarationish("ka*", parse = F))
  # parsing --> not declarationish
  expect_false(is_declarationish("ka*", parse = T))
})

test_that("anonymous checking works", {
  expect_true(is_anonymous(expected$one_sided))
  expect_false(is_anonymous(expected$two_sided))
})

test_that("empty checking works", {
  expect_false(is_empty_declaration("test"))
  expect_false(is_empty_declaration(expected$one_sided))
  expect_true(is_empty_declaration(declaration()))
})


test_that("listing of variable names works", {
  expect_equal(dec_vars(~ka*A+sin(x)+C["central"]), c("ka", "A", "x", "C"))
})

test_that("listing of functions works", {
  expect_equal(setdiff(
    dec_funs(~ka*A+exp(x)),
    c("*", "+", "exp")),
    character(0))
})
