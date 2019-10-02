context("NM model generation")

test_that("Assembly of NM models works", {
  m <- model()+
    prm_log_normal("emax")+
    prm_log_normal("ed50")+
    obs_additive(effect~emax*dose/(ed50+dose)) +
    parameter_values(emax = c(log_mu = 0.2, log_sigma = 0.1),
                     ed50 = c(log_mu = 0.3, log_sigma = 0.1))
  nm_code <-   as_nm_model(m) %>%
    render()

  expect_match(nm_code, "EMAX = THETA(1) * EXP(ETA(1))", fixed = TRUE, all = FALSE)
  expect_match(nm_code, "ED50 = THETA(2) * EXP(ETA(2))", fixed = TRUE, all = FALSE)
  expect_match(nm_code, "EFFECT = EMAX * DOSE/(ED50 + DOSE)", fixed = TRUE, all = FALSE)
  expect_match(nm_code, "Y = IPRED + EPS(1)", fixed = TRUE, all = FALSE)

})
