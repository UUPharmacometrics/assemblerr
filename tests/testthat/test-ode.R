context("ODE generation")

test_that("ODEs are generated correctly", {
  m <- model()+
    compartment("central", volume = ~vc) +
    flow(from = "central", definition = ~cl*C)
  ode_eqns <- generate_ode_equations(m)
  expect_named(ode_eqns, c("central"))
  expect_equal(fml_get_lhs(ode_eqns[[1]]), quote(dadt[1L]))
  expect_equal(fml_get_rhs(ode_eqns[[1]]), quote(cl * (A[1L]/vc)))
})
