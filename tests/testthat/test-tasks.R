test_that("estimation task", {
  m <- simple_model()
  render(m, tasks = tsk_estimation(algorithm = "foce")) %>%
    expect_contains("$ESTIMATION METHOD=COND") %>%
    expect_does_not_contain("INTERACTION")

  render(m, tasks = tsk_estimation(algorithm = "foce", se = TRUE)) %>%
    expect_contains("$ESTIMATION METHOD=COND") %>%
    expect_contains("$COVARIANCE")

  m2 <- simple_model(obs = obs_proportional(~conc))
  render(m2, tasks = tsk_estimation(algorithm = "foce")) %>%
    expect_match("\\$ESTIMATION METHOD=COND INTERACTION")
})

test_that("estimation task with additional options", {
  m <- simple_model()
  render(m, tasks = tsk_estimation(algorithm = "foce", target_options = list(test = 1))) %>%
    expect_match("\\$ESTIMATION .* TEST=1")

  render(m, tasks = tsk_estimation(algorithm = "saem", target_options = list(auto = FALSE))) %>%
    expect_match("\\$ESTIMATION METHOD=SAEM AUTO=0")

  render(m, tasks = tsk_estimation(algorithm = "saem", target_options = list(AUTO = FALSE))) %>%
    expect_match("\\$ESTIMATION METHOD=SAEM AUTO=0")
})
