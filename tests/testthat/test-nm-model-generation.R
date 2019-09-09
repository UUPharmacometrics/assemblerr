context("NM model generation")

test_that("Assembly of NM models works", {
  m <- model()+
    prm_log_normal("EMAX")+
    prm_log_normal("ED50")+
    obs_additive(effect~emax*dose/(ed50+dose))
  nm_code <- m %>%
    as_nm_model() %>%
    render()
  expect_match(nm_code, "EMAX = THETA(1) * EXP(ETA(1))", fixed = TRUE, all = FALSE)
  expect_match(nm_code, "ED50 = THETA(2) * EXP(ETA(2))", fixed = TRUE, all = FALSE)
  expect_match(nm_code, "EFFECT = EMAX * DOSE/(ED50 + DOSE)", fixed = TRUE, all = FALSE)
  expect_match(nm_code, "Y = IPRED + EPS(1)", fixed = TRUE, all = FALSE)

})
