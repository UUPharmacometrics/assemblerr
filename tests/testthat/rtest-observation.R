context("observation")

test_that("generation of concentration formulas works", {
  cmp <- list(name = "central", volume = ~1)
  cmp2 <- list(name = "central", volume = ~Vc + Vp)
  expect_equal(generate_conc_fml(cmp), C["central"] ~ A["central"]/1)
  expect_equal(generate_conc_fml(cmp2), C["central"] ~ A["central"]/(Vc + Vp))
})

test_that("replacing relative compartment references with absolute one works",{
  m <- model() +
    compartment("central")
  nmm <- nm_model() +
    nm_des("central", quote(dadt <- 1))
  fml <- replace_compartment_references(ipred~C["central"], nmm, m)
  expect_equal(fml, ipred ~ A[1L]/1)
})

test_that("generation of ipred formulas works",{
  m <- model() +
    compartment("central")
  nmm <- nm_model()
  obs <- list(name = "concentration", type = "additive", options = list(prediction = conc~exp(-t)))
  expect_equal(make_ipred_fml(nmm, m, obs), list(obs$options$prediction, ipred~conc))
})
