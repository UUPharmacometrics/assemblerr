
# compartment -------------------------------------------------------------


test_that("compartment", {
  expect_error(compartment())
  expect_error(compartment(1))
  expect_error(compartment("test cmp"))
  expect_silent(compartment("test", 1))
  expect_silent(compartment("test", ~vc))
})


# flow --------------------------------------------------------------------


test_that("flow", {
  expect_error(flow(~ka))
  expect_error(flow(~ka*A, to = "depot"))
  expect_error(flow(~ka*C, to = "depot"))
  expect_silent(flow(~ka*C, from = "depot"))
  expect_silent(flow(~ka, to = "central"))
})
