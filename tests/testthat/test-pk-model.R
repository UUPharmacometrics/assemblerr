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
    expect_contains("ADVAN1") %>%
    expect_contains("TRANS2") %>%
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
