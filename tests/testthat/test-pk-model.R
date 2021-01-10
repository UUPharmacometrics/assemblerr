context("test-pk-model")

expect_contains <- function(object, str) return(expect_match(object, str, fixed = TRUE, all = FALSE))

test_that("1cmp linear, advan", {
  m <- pk_model() +
    pk_distribution_1cmp() +
    pk_elimination_linear() +
    obs_additive(conc~C["central"])

  render(m) %>%
    expect_contains("VC = THETA(1) * EXP(ETA(1))") %>%
    expect_contains("CL = THETA(2) * EXP(ETA(2))") %>%
    expect_contains(" ADVAN1 ") %>%
    expect_contains(" TRANS2 ") %>%
    expect_contains("CONC = A(1)/VC") %>%
    expect_contains("Y = CONC + EPS(1)")
})

test_that("1cmp linear, ode", {
  m <- pk_model() +
    pk_distribution_1cmp() +
    pk_elimination_linear() +
    obs_additive(conc~C["central"])

  render(m,
         options = assemblerr_options(ode.use_special_advans = FALSE,
                                      ode.use_general_linear_advans = FALSE)
         ) %>%
    expect_contains("VC = THETA(1) * EXP(ETA(1))") %>%
    expect_contains("CL = THETA(2) * EXP(ETA(2))") %>%
    expect_contains("DADT(1) = -(CL * (A(1)/VC))") %>%
    expect_contains("CONC = A(1)/VC") %>%
    expect_contains("Y = CONC + EPS(1)")
})

test_that("2cmp linear", {
  m <- pk_model() +
    pk_distribution_2cmp() +
    pk_elimination_linear() +
    obs_additive(conc~C["central"])

  render(m,
         options = assemblerr_options(ode.use_special_advans = FALSE,
                                      ode.use_general_linear_advans = FALSE)
  ) %>%
    expect_contains("VC = THETA(1) * EXP(ETA(1))") %>%
    expect_contains("VP = THETA(2) * EXP(ETA(2))") %>%
    expect_contains("Q = THETA(3) * EXP(ETA(3))") %>%
    expect_contains("CL = THETA(4) * EXP(ETA(4))") %>%
    expect_contains("DADT(1) = Q * (A(2)/VP) - (Q * (A(1)/VC) + CL * (A(1)/VC))") %>%
    expect_contains("DADT(2) = Q * (A(1)/VC) - Q * (A(2)/VP)") %>%
    expect_contains("CONC = A(1)/VC") %>%
    expect_contains("Y = CONC + EPS(1)")

  render(m,
         options = assemblerr_options(ode.use_special_advans = TRUE,
                                      ode.use_general_linear_advans = FALSE)
  ) %>%
    expect_contains("VC = THETA(1) * EXP(ETA(1))") %>%
    expect_contains("VP = THETA(2) * EXP(ETA(2))") %>%
    expect_contains("Q = THETA(3) * EXP(ETA(3))") %>%
    expect_contains("CL = THETA(4) * EXP(ETA(4))") %>%
    expect_contains("V1 = VC") %>%
    expect_contains("V2 = VP") %>%
    expect_contains("CONC = A(1)/VC") %>%
    expect_contains("Y = CONC + EPS(1)")

  render(m,
         options = assemblerr_options(ode.use_special_advans = FALSE,
                                      ode.use_general_linear_advans = TRUE)
  ) %>%
    expect_contains("VC = THETA(1) * EXP(ETA(1))") %>%
    expect_contains("VP = THETA(2) * EXP(ETA(2))") %>%
    expect_contains("Q = THETA(3) * EXP(ETA(3))") %>%
    expect_contains("CL = THETA(4) * EXP(ETA(4))") %>%
    expect_contains("K12 = Q * (1/VC)") %>%
    expect_contains("K21 = Q * (1/VP)") %>%
    expect_contains("K10 = CL * (1/VC)") %>%
    expect_contains("CONC = A(1)/VC") %>%
    expect_contains("Y = CONC + EPS(1)")
})


test_that("1cmp linear 1st order absorption, advan", {
  m <- pk_model() +
    pk_absorption_fo() +
    pk_distribution_1cmp() +
    pk_elimination_linear() +
    obs_additive(conc~C["central"])

  render(m) %>%
    expect_contains(" ADVAN2 ") %>%
    expect_contains(" TRANS2 ") %>%
    expect_contains("MAT = THETA(1) * EXP(ETA(1))") %>%
    expect_contains("VC = THETA(2) * EXP(ETA(2))") %>%
    expect_contains("CL = THETA(3) * EXP(ETA(3))") %>%
    expect_contains("KA = 1/MAT") %>%
    expect_contains("CONC = A(2)/VC") %>%
    expect_contains("Y = CONC + EPS(1)")
})

test_that("3cmp linear", {
  m <- pk_model() +
    pk_distribution_3cmp() +
    pk_elimination_linear() +
    obs_additive(conc~C["central"])

  # =========== ODE ================================
  render(m,
         options = assemblerr_options(ode.use_special_advans = FALSE,
                                      ode.use_general_linear_advans = FALSE,
                                      ode.general_nonlinear_advan = "advan13")
  ) %>%
    expect_contains("$SUBROUTINES ADVAN13") %>%
    expect_contains("VC = THETA(1) * EXP(ETA(1))") %>%
    expect_contains("VP1 = THETA(2) * EXP(ETA(2))") %>%
    expect_contains("VP2 = THETA(3) * EXP(ETA(3))") %>%
    expect_contains("Q1 = THETA(4) * EXP(ETA(4))") %>%
    expect_contains("Q2 = THETA(5) * EXP(ETA(5))") %>%
    expect_contains("CL = THETA(6) * EXP(ETA(6))") %>%
    expect_contains("DADT(1) = Q1 * (A(2)/VP1) + Q2 * (A(3)/VP2) - (Q1 * (A(1)/VC) + Q2 * (A(1)/VC) + CL * (A(1)/VC))") %>%
    expect_contains("DADT(2) = Q1 * (A(1)/VC) - Q1 * (A(2)/VP1)") %>%
    expect_contains("DADT(3) = Q2 * (A(1)/VC) - Q2 * (A(3)/VP2)") %>%
    expect_contains("CONC = A(1)/VC") %>%
    expect_contains("Y = CONC + EPS(1)")

  # =========== ADVAN 11 ================================
  render(m,
         options = assemblerr_options(ode.use_special_advans = TRUE,
                                      ode.use_general_linear_advans = FALSE)
  ) %>%
    expect_contains("$SUBROUTINES ADVAN11") %>%
    expect_contains("VC = THETA(1) * EXP(ETA(1))") %>%
    expect_contains("VP1 = THETA(2) * EXP(ETA(2))") %>%
    expect_contains("VP2 = THETA(3) * EXP(ETA(3))") %>%
    expect_contains("Q1 = THETA(4) * EXP(ETA(4))") %>%
    expect_contains("Q2 = THETA(5) * EXP(ETA(5))") %>%
    expect_contains("CL = THETA(6) * EXP(ETA(6))") %>%
    expect_contains("V1 = VC") %>%
    expect_contains("V2 = VP1") %>%
    expect_contains("V3 = VP2") %>%
    expect_contains("CONC = A(1)/VC") %>%
    expect_contains("Y = CONC + EPS(1)")

  # =========== ADVAN 5 ================================

  render(m,
         options = assemblerr_options(ode.use_special_advans = FALSE,
                                      ode.use_general_linear_advans = TRUE)
  ) %>%
    expect_contains("$SUBROUTINES ADVAN5") %>%
    expect_contains("VC = THETA(1) * EXP(ETA(1))") %>%
    expect_contains("VP1 = THETA(2) * EXP(ETA(2))") %>%
    expect_contains("VP2 = THETA(3) * EXP(ETA(3))") %>%
    expect_contains("Q1 = THETA(4) * EXP(ETA(4))") %>%
    expect_contains("Q2 = THETA(5) * EXP(ETA(5))") %>%
    expect_contains("CL = THETA(6) * EXP(ETA(6))") %>%
    expect_contains("K12 = Q1 * (1/VC)") %>%
    expect_contains("K21 = Q1 * (1/VP1)") %>%
    expect_contains("K13 = Q2 * (1/VC)") %>%
    expect_contains("K31 = Q2 * (1/VP2)") %>%
    expect_contains("K10 = CL * (1/VC)") %>%
    expect_contains("CONC = A(1)/VC") %>%
    expect_contains("Y = CONC + EPS(1)")
})


test_that("1cmp linear, transit delay", {
  m <- pk_model() +
    pk_distribution_1cmp() +
    pk_elimination_linear() +
    pk_absorption_fo_transit(transit_compartments = 3) +
    obs_additive(conc~C["central"])

  render(m,
         options = assemblerr_options(
           ode.use_special_advans = TRUE,
           ode.use_general_linear_advans = TRUE
          )
    ) %>%
    expect_contains(" ADVAN5 ") %>%
    expect_contains(" TRANS1 ") %>%
    expect_contains("VC = THETA(1) * EXP(ETA(1))") %>%
    expect_contains("CL = THETA(2) * EXP(ETA(2))") %>%
    expect_contains("MDT = THETA(3) * EXP(ETA(3))") %>%
    expect_contains("MAT = THETA(4) * EXP(ETA(4))") %>%
    expect_contains("KTR = 3/MDT") %>%
    expect_contains("KA = 1/MAT") %>%
    expect_contains("K10 = CL * (1/VC)") %>%
    expect_contains("K23 = KTR") %>%
    expect_contains("K34 = KTR") %>%
    expect_contains("K41 = KA") %>%
    expect_contains("CONC = A(1)/VC") %>%
    expect_contains("Y = CONC + EPS(1)")
})
