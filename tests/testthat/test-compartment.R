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

  outflow <- m$compartments[[1]]$outflows[[1]]
  expect_equal(outflow$from, "depot")
  expect_equal(outflow$to, "central")
  expect_equal(outflow$equation, quote(ka*C))

  inflow <- m$compartments[[2]]$inflows[[1]]
  expect_equal(inflow$from, "depot")
  expect_equal(inflow$to, "central")
  expect_equal(inflow$equation, quote(ka*C))
})

test_that("adding a non-mass-balanced flow adds only one flow", {
  m <- Model() %>%
    add_compartment("depot") %>%
    add_compartment("central") %>%
    add_flow(from = "central", equation = ~Cl*C)

  outflow <- m$compartments[[2]]$outflows[[1]]
  expect_equal(outflow$from, "central")
  expect_equal(outflow$to, NULL)
  expect_equal(outflow$equation, quote(Cl*C))
})
