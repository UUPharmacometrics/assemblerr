local_edition(3)
local_reproducible_output()

# individual building blocks ----------------------------------------------

test_that("parameters", {
  for (b in lsf.str("package:assemblerr", pattern = "^prm_")) {
    expect_snapshot(
      (!!b)("test")
    )
  }
})

test_that("observations", {
  for (b in lsf.str("package:assemblerr", pattern = "^obs_")) {
    expect_snapshot(
      (!!b)(~test)
    )
  }
})

test_that("compartment, flow", {
  expect_snapshot(
    flow(~k, "central")
  )
  expect_snapshot(
    flow(~k, "depot", "central")
  )
  expect_snapshot(
    compartment("central")
  )
})

test_that("algebraics", {
  expect_snapshot(
    algebraic(k~cl/v)
  )
})

# model -------------------------------------------------------------------

test_that("empty model", {
  expect_snapshot(
    model()
  )
})

test_that("permuation of prm and obs", {
  blocks <- list(
    prm = lsf.str("package:assemblerr", pattern = "^prm_"),
    obs = lsf.str("package:assemblerr", pattern = "^obs_")
  ) %>%
    purrr::map(rlang::syms) %>%
    purrr::cross()
  for (b in blocks) {
    expect_snapshot(
      model() +
        (!!b$prm)("k") +
        compartment("central") +
        flow(~k*C, "central") +
        (!!b$obs)(~C["central"])
    )
  }
})


# pk_model ----------------------------------------------------------------

test_that("empty pk_model", {
  expect_snapshot(pk_model())
})

test_that("permutation of PK models",{
  pk_components <- list(
    distribution = lsf.str("package:assemblerr", pattern = "pk_distribution"),
    elimination = lsf.str("package:assemblerr", pattern = "pk_elimination"),
    absorption = lsf.str("package:assemblerr", pattern = "pk_absorption")
    ) %>%
    purrr::map(rlang::syms) %>%
    purrr::cross()
  for (pkc in pk_components) {
    expect_snapshot(
      pk_model() +
        (!!pkc$distribution)() +
        (!!pkc$elimination)() +
        (!!pkc$absorption)() +
        obs_additive(~C["central"])
    )
  }

})
