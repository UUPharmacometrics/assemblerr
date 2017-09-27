context("model compartment system")

test_that("adding of compartments to a model works", {
  m <- Model() %>% add_compartment("test", 2)
  expect_equal(m$compartments[["test"]]$volume, 2)
})

test_that("can add compartment without volume", {
  m <- Model() %>% add_compartment("test")
  expect_equal(m$compartments[["test"]]$volume, 1)
})

test_that("compartment volume can be parameter", {
  m <- Model() %>% add_compartment("test", volume = ~V)
  expect_equal(m$compartments[["test"]]$volume, quote(V))
})



test_that("adding a mass-balanced flow to a model works", {
  m <- Model() %>%
    add_compartment("depot") %>%
    add_compartment("central") %>%
    add_flow(from = "depot", to = "central", equation = ~ka*C)

  expect_equal(m$flows[[1]]$from, "depot")
  expect_equal(m$flows[[1]]$to, "central")
  expect_equal(m$flows[[1]]$equation, quote(ka*C))

  expect_equal(m$flows[[2]]$from, "central")
  expect_equal(m$flows[[2]]$to, "depot")
  expect_equal(m$flows[[2]]$equation, quote(-(ka*C)))
})

test_that("adding a non-mass-balanced flow adds only one flow", {
  m <- Model() %>%
    add_compartment("depot") %>%
    add_compartment("central") %>%
    add_flow(from = "depot", to = "central", equation = ~ka*C, mass_balance = F)

  expect_equal(m$flows[[1]]$from, "depot")
  expect_equal(m$flows[[1]]$to, "central")
  expect_equal(m$flows[[1]]$equation, quote(ka*C))
  expect_equal(length(m$flows), 1)

})
