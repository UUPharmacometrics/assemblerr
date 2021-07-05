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

test_that("output task parameters", {
  m <- simple_model()
  render(m, tasks = tsk_output("sdtab", variables = vars_prms())) %>%
    expect_match("\\$TABLE.*K.*FILE=sdtab")
})

test_that("output task standard variables", {
  m <- simple_model()
  render(m, tasks = tsk_output("sdtab", variables = vars_nm_std())) %>%
    expect_match("\\$TABLE.*DV") %>%
    expect_match("\\$TABLE.*PRED") %>%
    expect_match("\\$TABLE.*RES") %>%
    expect_match("\\$TABLE.*WRES") %>%
    expect_match("\\$TABLE.*IPREDI") %>%
    expect_match("\\$TABLE.*IWRESI")
})

test_that("output task eta variables", {
  m <- simple_model()
  render(m, tasks = tsk_output("sdtab", variables = vars_eta())) %>%
    expect_match("\\$TABLE.*ETA\\(1\\)")
})

test_that("output task data variables", {
  m <- simple_model()
  render(m, tasks = tsk_output("sdtab", variables = vars_data())) %>%
    expect_match("\\$TABLE.*TIME") %>%
    expect_match("\\$TABLE.*C0")
})

test_that("xpose4 output task ", {
  m <- simple_model()
  render(m, tasks = tsk_output_xpose4()) %>%
    expect_match("\\$TABLE.*ID.*FILE=sdtab") %>%
    expect_match("\\$TABLE.*TIME.*FILE=sdtab") %>%
    expect_match("\\$TABLE.*DV.*FILE=sdtab") %>%
    expect_match("\\$TABLE.*PRED.*FILE=sdtab") %>%
    expect_match("\\$TABLE.*RES.*FILE=sdtab") %>%
    expect_match("\\$TABLE.*WRES.*FILE=sdtab") %>%
    expect_match("\\$TABLE.*IPREDI.*FILE=sdtab") %>%
    expect_match("\\$TABLE.*IWRESI.*FILE=sdtab") %>%
    expect_match("\\$TABLE.*K.*FILE=patab") %>%
    expect_match("\\$TABLE.*ETA\\(1\\).*FILE=patab")
})
