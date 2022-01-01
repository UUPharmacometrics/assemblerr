local_edition(3)
local_reproducible_output()

# Parameters ----------------------------------------------------------------------
#
# Automatically generated with:
# 	create_snapshot_tests(test_name = "{fn}", test_expr = "{fn}('test')", fn = list_fns("^prm_"), section_label = "Parameters")

test_that("prm_log_normal", {
  expect_snapshot(
    prm_log_normal('test')
  )
})

test_that("prm_logit_normal", {
  expect_snapshot(
    prm_logit_normal('test')
  )
})

test_that("prm_no_var", {
  expect_snapshot(
    prm_no_var('test')
  )
})

test_that("prm_normal", {
  expect_snapshot(
    prm_normal('test')
  )
})

# Observations --------------------------------------------------------------------
#
# Automatically generated with:
# 	create_snapshot_tests(test_name = "{fn}", test_expr = "{fn}(~test)", fn = list_fns("^obs_"), section_label = "Observations")

test_that("obs_additive", {
  expect_snapshot(
    obs_additive(~test)
  )
})

test_that("obs_combined", {
  expect_snapshot(
    obs_combined(~test)
  )
})

test_that("obs_proportional", {
  expect_snapshot(
    obs_proportional(~test)
  )
})


# # Compartment -----------------------------------------------------------



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

# Algebraics --------------------------------------------------------------


test_that("algebraics", {
  expect_snapshot(
    algebraic(k~cl/v)
  )
})

# Model -------------------------------------------------------------------

test_that("empty model", {
  expect_snapshot(
    model()
  )
})

# Parameter + Observation ---------------------------------------------------------
#
# Automatically generated with:
# 	create_snapshot_tests(test_name = "{prm} + {obs}",
#    test_expr = "model() +
#     {prm}('k') +
#     compartment('central') +
#     flow(~k*C, 'central') +
#     {obs}(~C['central'])",
# 	  !!!list_combs(prm = "^prm_", obs = "^obs_"),
#      section_label = "Parameter + Observation"
#   )

test_that("prm_log_normal + obs_additive", {
  expect_snapshot(
    model() +
      prm_log_normal('k') +
      compartment('central') +
      flow(~k*C, 'central') +
      obs_additive(~C['central'])
  )
})

test_that("prm_logit_normal + obs_additive", {
  expect_snapshot(
    model() +
      prm_logit_normal('k') +
      compartment('central') +
      flow(~k*C, 'central') +
      obs_additive(~C['central'])
  )
})

test_that("prm_no_var + obs_additive", {
  expect_snapshot(
    model() +
      prm_no_var('k') +
      compartment('central') +
      flow(~k*C, 'central') +
      obs_additive(~C['central'])
  )
})

test_that("prm_normal + obs_additive", {
  expect_snapshot(
    model() +
      prm_normal('k') +
      compartment('central') +
      flow(~k*C, 'central') +
      obs_additive(~C['central'])
  )
})

test_that("prm_log_normal + obs_combined", {
  expect_snapshot(
    model() +
      prm_log_normal('k') +
      compartment('central') +
      flow(~k*C, 'central') +
      obs_combined(~C['central'])
  )
})

test_that("prm_logit_normal + obs_combined", {
  expect_snapshot(
    model() +
      prm_logit_normal('k') +
      compartment('central') +
      flow(~k*C, 'central') +
      obs_combined(~C['central'])
  )
})

test_that("prm_no_var + obs_combined", {
  expect_snapshot(
    model() +
      prm_no_var('k') +
      compartment('central') +
      flow(~k*C, 'central') +
      obs_combined(~C['central'])
  )
})

test_that("prm_normal + obs_combined", {
  expect_snapshot(
    model() +
      prm_normal('k') +
      compartment('central') +
      flow(~k*C, 'central') +
      obs_combined(~C['central'])
  )
})

test_that("prm_log_normal + obs_proportional", {
  expect_snapshot(
    model() +
      prm_log_normal('k') +
      compartment('central') +
      flow(~k*C, 'central') +
      obs_proportional(~C['central'])
  )
})

test_that("prm_logit_normal + obs_proportional", {
  expect_snapshot(
    model() +
      prm_logit_normal('k') +
      compartment('central') +
      flow(~k*C, 'central') +
      obs_proportional(~C['central'])
  )
})

test_that("prm_no_var + obs_proportional", {
  expect_snapshot(
    model() +
      prm_no_var('k') +
      compartment('central') +
      flow(~k*C, 'central') +
      obs_proportional(~C['central'])
  )
})

test_that("prm_normal + obs_proportional", {
  expect_snapshot(
    model() +
      prm_normal('k') +
      compartment('central') +
      flow(~k*C, 'central') +
      obs_proportional(~C['central'])
  )
})


# pk_model ----------------------------------------------------------------

test_that("empty pk_model", {
  expect_snapshot(pk_model())
})

# PK model + components -----------------------------------------------------------
#
# Automatically generated with:
# 	create_snapshot_tests(test_name = "{d} + {e} + {a}", test_expr = "pk_model() + {d}() + {e}() + {a}() + obs_additive(~C['central'])",
# 	  !!!list_combs(d = "pk_distribution", e = "pk_elimination", a = "pk_absorption"), section_label = "PK model + components")

test_that("pk_distribution_1cmp + pk_elimination_linear + pk_absorption_fo", {
  expect_snapshot(
    pk_model() + pk_distribution_1cmp() + pk_elimination_linear() + pk_absorption_fo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_2cmp + pk_elimination_linear + pk_absorption_fo", {
  expect_snapshot(
    pk_model() + pk_distribution_2cmp() + pk_elimination_linear() + pk_absorption_fo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_3cmp + pk_elimination_linear + pk_absorption_fo", {
  expect_snapshot(
    pk_model() + pk_distribution_3cmp() + pk_elimination_linear() + pk_absorption_fo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_1cmp + pk_elimination_linear_nl + pk_absorption_fo", {
  expect_snapshot(
    pk_model() + pk_distribution_1cmp() + pk_elimination_linear_nl() + pk_absorption_fo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_2cmp + pk_elimination_linear_nl + pk_absorption_fo", {
  expect_snapshot(
    pk_model() + pk_distribution_2cmp() + pk_elimination_linear_nl() + pk_absorption_fo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_3cmp + pk_elimination_linear_nl + pk_absorption_fo", {
  expect_snapshot(
    pk_model() + pk_distribution_3cmp() + pk_elimination_linear_nl() + pk_absorption_fo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_1cmp + pk_elimination_nl + pk_absorption_fo", {
  expect_snapshot(
    pk_model() + pk_distribution_1cmp() + pk_elimination_nl() + pk_absorption_fo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_2cmp + pk_elimination_nl + pk_absorption_fo", {
  expect_snapshot(
    pk_model() + pk_distribution_2cmp() + pk_elimination_nl() + pk_absorption_fo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_3cmp + pk_elimination_nl + pk_absorption_fo", {
  expect_snapshot(
    pk_model() + pk_distribution_3cmp() + pk_elimination_nl() + pk_absorption_fo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_1cmp + pk_elimination_linear + pk_absorption_fo_lag", {
  expect_snapshot(
    pk_model() + pk_distribution_1cmp() + pk_elimination_linear() + pk_absorption_fo_lag() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_2cmp + pk_elimination_linear + pk_absorption_fo_lag", {
  expect_snapshot(
    pk_model() + pk_distribution_2cmp() + pk_elimination_linear() + pk_absorption_fo_lag() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_3cmp + pk_elimination_linear + pk_absorption_fo_lag", {
  expect_snapshot(
    pk_model() + pk_distribution_3cmp() + pk_elimination_linear() + pk_absorption_fo_lag() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_1cmp + pk_elimination_linear_nl + pk_absorption_fo_lag", {
  expect_snapshot(
    pk_model() + pk_distribution_1cmp() + pk_elimination_linear_nl() + pk_absorption_fo_lag() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_2cmp + pk_elimination_linear_nl + pk_absorption_fo_lag", {
  expect_snapshot(
    pk_model() + pk_distribution_2cmp() + pk_elimination_linear_nl() + pk_absorption_fo_lag() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_3cmp + pk_elimination_linear_nl + pk_absorption_fo_lag", {
  expect_snapshot(
    pk_model() + pk_distribution_3cmp() + pk_elimination_linear_nl() + pk_absorption_fo_lag() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_1cmp + pk_elimination_nl + pk_absorption_fo_lag", {
  expect_snapshot(
    pk_model() + pk_distribution_1cmp() + pk_elimination_nl() + pk_absorption_fo_lag() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_2cmp + pk_elimination_nl + pk_absorption_fo_lag", {
  expect_snapshot(
    pk_model() + pk_distribution_2cmp() + pk_elimination_nl() + pk_absorption_fo_lag() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_3cmp + pk_elimination_nl + pk_absorption_fo_lag", {
  expect_snapshot(
    pk_model() + pk_distribution_3cmp() + pk_elimination_nl() + pk_absorption_fo_lag() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_1cmp + pk_elimination_linear + pk_absorption_fo_transit", {
  expect_snapshot(
    pk_model() + pk_distribution_1cmp() + pk_elimination_linear() + pk_absorption_fo_transit() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_2cmp + pk_elimination_linear + pk_absorption_fo_transit", {
  expect_snapshot(
    pk_model() + pk_distribution_2cmp() + pk_elimination_linear() + pk_absorption_fo_transit() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_3cmp + pk_elimination_linear + pk_absorption_fo_transit", {
  expect_snapshot(
    pk_model() + pk_distribution_3cmp() + pk_elimination_linear() + pk_absorption_fo_transit() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_1cmp + pk_elimination_linear_nl + pk_absorption_fo_transit", {
  expect_snapshot(
    pk_model() + pk_distribution_1cmp() + pk_elimination_linear_nl() + pk_absorption_fo_transit() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_2cmp + pk_elimination_linear_nl + pk_absorption_fo_transit", {
  expect_snapshot(
    pk_model() + pk_distribution_2cmp() + pk_elimination_linear_nl() + pk_absorption_fo_transit() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_3cmp + pk_elimination_linear_nl + pk_absorption_fo_transit", {
  expect_snapshot(
    pk_model() + pk_distribution_3cmp() + pk_elimination_linear_nl() + pk_absorption_fo_transit() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_1cmp + pk_elimination_nl + pk_absorption_fo_transit", {
  expect_snapshot(
    pk_model() + pk_distribution_1cmp() + pk_elimination_nl() + pk_absorption_fo_transit() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_2cmp + pk_elimination_nl + pk_absorption_fo_transit", {
  expect_snapshot(
    pk_model() + pk_distribution_2cmp() + pk_elimination_nl() + pk_absorption_fo_transit() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_3cmp + pk_elimination_nl + pk_absorption_fo_transit", {
  expect_snapshot(
    pk_model() + pk_distribution_3cmp() + pk_elimination_nl() + pk_absorption_fo_transit() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_1cmp + pk_elimination_linear + pk_absorption_fo_zo", {
  expect_snapshot(
    pk_model() + pk_distribution_1cmp() + pk_elimination_linear() + pk_absorption_fo_zo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_2cmp + pk_elimination_linear + pk_absorption_fo_zo", {
  expect_snapshot(
    pk_model() + pk_distribution_2cmp() + pk_elimination_linear() + pk_absorption_fo_zo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_3cmp + pk_elimination_linear + pk_absorption_fo_zo", {
  expect_snapshot(
    pk_model() + pk_distribution_3cmp() + pk_elimination_linear() + pk_absorption_fo_zo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_1cmp + pk_elimination_linear_nl + pk_absorption_fo_zo", {
  expect_snapshot(
    pk_model() + pk_distribution_1cmp() + pk_elimination_linear_nl() + pk_absorption_fo_zo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_2cmp + pk_elimination_linear_nl + pk_absorption_fo_zo", {
  expect_snapshot(
    pk_model() + pk_distribution_2cmp() + pk_elimination_linear_nl() + pk_absorption_fo_zo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_3cmp + pk_elimination_linear_nl + pk_absorption_fo_zo", {
  expect_snapshot(
    pk_model() + pk_distribution_3cmp() + pk_elimination_linear_nl() + pk_absorption_fo_zo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_1cmp + pk_elimination_nl + pk_absorption_fo_zo", {
  expect_snapshot(
    pk_model() + pk_distribution_1cmp() + pk_elimination_nl() + pk_absorption_fo_zo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_2cmp + pk_elimination_nl + pk_absorption_fo_zo", {
  expect_snapshot(
    pk_model() + pk_distribution_2cmp() + pk_elimination_nl() + pk_absorption_fo_zo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_3cmp + pk_elimination_nl + pk_absorption_fo_zo", {
  expect_snapshot(
    pk_model() + pk_distribution_3cmp() + pk_elimination_nl() + pk_absorption_fo_zo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_1cmp + pk_elimination_linear + pk_absorption_zo", {
  expect_snapshot(
    pk_model() + pk_distribution_1cmp() + pk_elimination_linear() + pk_absorption_zo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_2cmp + pk_elimination_linear + pk_absorption_zo", {
  expect_snapshot(
    pk_model() + pk_distribution_2cmp() + pk_elimination_linear() + pk_absorption_zo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_3cmp + pk_elimination_linear + pk_absorption_zo", {
  expect_snapshot(
    pk_model() + pk_distribution_3cmp() + pk_elimination_linear() + pk_absorption_zo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_1cmp + pk_elimination_linear_nl + pk_absorption_zo", {
  expect_snapshot(
    pk_model() + pk_distribution_1cmp() + pk_elimination_linear_nl() + pk_absorption_zo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_2cmp + pk_elimination_linear_nl + pk_absorption_zo", {
  expect_snapshot(
    pk_model() + pk_distribution_2cmp() + pk_elimination_linear_nl() + pk_absorption_zo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_3cmp + pk_elimination_linear_nl + pk_absorption_zo", {
  expect_snapshot(
    pk_model() + pk_distribution_3cmp() + pk_elimination_linear_nl() + pk_absorption_zo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_1cmp + pk_elimination_nl + pk_absorption_zo", {
  expect_snapshot(
    pk_model() + pk_distribution_1cmp() + pk_elimination_nl() + pk_absorption_zo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_2cmp + pk_elimination_nl + pk_absorption_zo", {
  expect_snapshot(
    pk_model() + pk_distribution_2cmp() + pk_elimination_nl() + pk_absorption_zo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_3cmp + pk_elimination_nl + pk_absorption_zo", {
  expect_snapshot(
    pk_model() + pk_distribution_3cmp() + pk_elimination_nl() + pk_absorption_zo() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_1cmp + pk_elimination_linear + pk_absorption_zo_lag", {
  expect_snapshot(
    pk_model() + pk_distribution_1cmp() + pk_elimination_linear() + pk_absorption_zo_lag() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_2cmp + pk_elimination_linear + pk_absorption_zo_lag", {
  expect_snapshot(
    pk_model() + pk_distribution_2cmp() + pk_elimination_linear() + pk_absorption_zo_lag() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_3cmp + pk_elimination_linear + pk_absorption_zo_lag", {
  expect_snapshot(
    pk_model() + pk_distribution_3cmp() + pk_elimination_linear() + pk_absorption_zo_lag() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_1cmp + pk_elimination_linear_nl + pk_absorption_zo_lag", {
  expect_snapshot(
    pk_model() + pk_distribution_1cmp() + pk_elimination_linear_nl() + pk_absorption_zo_lag() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_2cmp + pk_elimination_linear_nl + pk_absorption_zo_lag", {
  expect_snapshot(
    pk_model() + pk_distribution_2cmp() + pk_elimination_linear_nl() + pk_absorption_zo_lag() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_3cmp + pk_elimination_linear_nl + pk_absorption_zo_lag", {
  expect_snapshot(
    pk_model() + pk_distribution_3cmp() + pk_elimination_linear_nl() + pk_absorption_zo_lag() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_1cmp + pk_elimination_nl + pk_absorption_zo_lag", {
  expect_snapshot(
    pk_model() + pk_distribution_1cmp() + pk_elimination_nl() + pk_absorption_zo_lag() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_2cmp + pk_elimination_nl + pk_absorption_zo_lag", {
  expect_snapshot(
    pk_model() + pk_distribution_2cmp() + pk_elimination_nl() + pk_absorption_zo_lag() + obs_additive(~C['central'])
  )
})

test_that("pk_distribution_3cmp + pk_elimination_nl + pk_absorption_zo_lag", {
  expect_snapshot(
    pk_model() + pk_distribution_3cmp() + pk_elimination_nl() + pk_absorption_zo_lag() + obs_additive(~C['central'])
  )
})
