expect_contains <- function(object, str) return(expect_match(object, str, fixed = TRUE, all = FALSE))

test_that("parameters", {
  m <- model()
  m2 <- m + prm_log_normal("test")
  expect_matching_issue(check(m), "No parameters")
  expect_no_matching_issue(check(m2), "No parameters")
})

test_that("observation", {
  m <- model()
  m2 <- m + obs_additive(~test)
  expect_matching_issue(check(m), "No observation")
  expect_no_matching_issue(check(m2), "No observation")
})

test_that("missing variables in algebraics", {
  m <- model() +
    algebraic(auc~dose/cl)
  m2 <- m +
    prm_log_normal("cl") +
    input_variable("dose")
  expect_matching_issue(check(m), "Undefined variables.*algebraics")
  expect_no_matching_issue(check(m2), "Undefined variables.*algebraics")
})

test_that("missing variables in observation", {
  m <- model() +
    obs_additive(~dose/cl)
  m2 <- m +
    prm_log_normal("cl") +
    input_variable("dose")
  m3 <- model() +
    obs_additive(~C["central"])
  m4 <- m3 +
    compartment("central")
  expect_matching_issue(check(m), "Undefined variables.*observation")
  expect_no_matching_issue(check(m2), "Undefined variables.*observation")
  expect_matching_issue(check(m3), "Undefined variable.*observation")
  expect_no_matching_issue(check(m4), "Undefined variable.*observation")

})

test_that("missing compartment names in flow definition", {
  m <- model() +
    flow(~ka, from = "test")
  expect_matching_issue(check(m), "Undefined compartment name 'test' in flow definition")
})



test_that("missing variables in flow definition", {
  m <- model() +
    flow(~ka, from = "test")
  expect_matching_issue(check(m), "Undefined variable.*flows")
})


test_that("missing volume variables in compartment", {
  m <- model() +
    compartment("central", ~v)
  expect_matching_issue(check(m), "Undefined variable.*compartment")
})

test_that("adding of missing variables", {
  m <- model() +
    prm_log_normal("v") +
    prm_log_normal("cl") +
    algebraic(d~dose) +
    obs_additive(conc~d/v*exp(-cl/v*time))
  expect_warning(
    code <- render(m)
  )
  expect_match(code, "\\$INPUT DOSE TIME")
  expect_silent(
    code <- render(m, options = assemblerr_options(issues.missing_variables = "fix"))
  )
  expect_match(code, "\\$INPUT DOSE TIME")
  expect_error(
    code <- render(m, options = assemblerr_options(issues.missing_variables = "fail"))
  )
  expect_silent(
    code <- render(m, options = assemblerr_options(issues.missing_variables = "ignore"))
  )
  expect_match(code, "\\$INPUT ((?!DOSE).)*", perl = TRUE)

})
