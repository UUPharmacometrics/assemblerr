test_that("renaming in analytic model", {
  simple_model() %>%
    rename_variables(c(k = "k1")) %>%
    render() %>%
    expect_contains("K1 = THETA(1) * EXP(ETA(1))") %>%
    expect_contains("CONC = C0 * EXP(-K1 * TIME)")

  simple_model(obs = obs_additive(c~conc)) %>%
    rename_variables(c(conc = "conc2")) %>%
    render() %>%
    expect_contains("C = CONC2")
})


test_that("renaming in ode model", {
  simple_ode_model() %>%
    rename_variables(c(cl = "cl2")) %>%
    render() %>%
    expect_contains("CL2 = ") %>%
    expect_contains("CL = CL2")
})


test_that("renaming in PK model", {
  simple_pk_model() %>%
    rename_variables(c(cl = "cl2")) %>%
    render() %>%
    expect_contains("CL2 = ") %>%
    expect_contains("CL = CL2")
})

test_that("model rendering", {
  m <- simple_model() +
    prm_normal("K")

  expect_warning(render(m), "Variables renamed") %>%
    expect_contains("K1 = ")
})

test_that("pk_model rendering", {
  m <- simple_pk_model() +
    prm_normal("Mat")

  expect_warning(render(m), "Variables renamed") %>%
    expect_contains("MAT1 = ")
})
