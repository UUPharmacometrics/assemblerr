context("test-model")


expect_contains <- function(object, str) return(expect_match(object, str, fixed = TRUE, all = FALSE))

simple_model <- function(prm = prm_log_normal("k"), obs = obs_additive(c~conc)) {
  model() +
    prm +
    algebraic(conc~c0*exp(-k*time)) +
    obs
}

test_that("check empty model",{
  local_edition(3)
  local_reproducible_output()
  m <- model()
  expect_snapshot(print(check(m)))
})

test_that("check simple model",{
  local_edition(3)
  local_reproducible_output()
  m <- simple_model()
  expect_snapshot(print(check(m)))
})


test_that("required crtl records", {
  simple_model() %>%
    render() %>%
    expect_contains("$PROBLEM") %>%
    expect_contains("$INPUT") %>%
    expect_contains("$PRED")
})

test_that("prm normal distribution", {
  simple_model(prm = prm_normal("k")) %>%
    render(options = assemblerr_options(prm.use_mu_referencing = TRUE)) %>%
    expect_contains("MU_1 = THETA(1)") %>%
    expect_contains("K = THETA(1) + ETA(1)") %>%
    expect_contains("$OMEGA") %>%
    expect_contains("$THETA")
})

test_that("prm log-normal distribution", {
  simple_model(prm = prm_log_normal("k")) %>%
    render(options = assemblerr_options(prm.use_mu_referencing = TRUE)) %>%
    expect_contains("MU_1 = LOG(THETA(1))") %>%
    expect_contains("K = THETA(1) * EXP(ETA(1))") %>%
    expect_contains("$OMEGA") %>%
    expect_contains("$THETA")
})

test_that("prm novar distribution", {
  simple_model(prm = prm_no_var("k")) %>%
  render(options = assemblerr_options(prm.use_mu_referencing = TRUE)) %>%
    expect_contains("K = THETA(1)") %>%
    expect_contains("$THETA")
})

test_that("prm logit distribution", {
  simple_model(prm = prm_logit_normal("k")) %>%
    render(options = assemblerr_options(prm.use_mu_referencing = TRUE)) %>%
    expect_contains("LOGIT_K = LOG(THETA(1)/(1 - THETA(1))) + ETA(1)") %>%
    expect_contains("K = EXP(LOGIT_K)/(1 + EXP(LOGIT_K))") %>%
    expect_contains("$THETA")
})


test_that("obs additive", {
  simple_model(obs = obs_additive(c ~ conc)) %>%
    render() %>%
    expect_contains("C = CONC") %>%
    expect_contains("Y = C + EPS(1)") %>%
    expect_contains("$SIGMA")
})

test_that("obs proportional", {
  simple_model(obs = obs_proportional(c ~ conc)) %>%
    render() %>%
    expect_contains("C = CONC") %>%
    expect_contains("Y = C + C * EPS(1)") %>%
    expect_contains("$SIGMA")
})

test_that("obs combined", {
  simple_model(obs = obs_combined(c ~ conc)) %>%
    render() %>%
    expect_contains("C = CONC") %>%
    expect_contains("Y = C + EPS(1) + C * EPS(2)") %>%
    expect_contains("$SIGMA")
})

test_that("adding variables from a dataset", {
  m <- simple_model()
  local_create_nonmem_test_directory()
  df <- data.frame(ID = 1, TIME = 1, DV = 1, DOSE = 1)
  write.csv(df, "data.csv", quote = FALSE, row.names = FALSE)
  m <- m +
    dataset(path = "data.csv")
  render(m) %>%
    expect_contains("$INPUT ID TIME DV DOSE") %>%
    expect_contains("$DATA data.csv IGNORE=@")
})


test_that("flow error checking", {
  expect_error(flow(~ka))
  expect_error(flow(~ka*A, to = "depot"))
  expect_error(flow(~ka*C, to = "depot"))
  expect_silent(flow(~ka*C, from = "depot"))
  expect_silent(flow(~ka, to = "central"))
})
