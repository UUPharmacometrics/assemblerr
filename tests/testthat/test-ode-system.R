context("Test ODE system")

test_that("creation of a 1 comp system yields the expected equations",{
  ode <- ODESystem()
  central <- ode$request_compartment("central", quote(V))
  ode$add_flow(from = central, equation = quote(CL*C))

  expect_equal(ode$get_des_code()$central,
               quote(dA_dt[1L] <- -(CL * (A[1L]/V))))
})


test_that("creation of a 2 comp 1st order-absorption system yields the expected equations",{
  ode <- ODESystem()
  central <- ode$request_compartment("central", quote(V))
  dose <- ode$request_compartment("dose", 1)
  peri <- ode$request_compartment("peri", quote(VP))

  ode$add_flow(from = central, equation = quote(CL*C))
  ode$add_flow(from = dose, to = central, equation = quote(ka*C))

  ode$add_flow(from = central, to = peri, equation = quote(Q*C))
  ode$add_flow(from = peri, to = central, equation = quote(Q*C))

  expect_equal(ode$get_des_code()$central,
               quote(dA_dt[1L] <- -(CL * (A[1L]/V)) + ka *(A[2L]/1) + - (Q * (A[1L]/V)) +
                       Q * (A[3L]/VP)))
  expect_equal(ode$get_des_code()$dose,
               quote(dA_dt[2L] <- - (ka *(A[2L]/1))))
  expect_equal(ode$get_des_code()$peri,
               quote(dA_dt[3L] <- Q * (A[1L]/V) + -(Q * (A[3L]/VP))))

})
