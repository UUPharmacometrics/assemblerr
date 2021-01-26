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

test_that("missing variables", {
  m <- model() +
    algebraic(auc~dose/cl)
  m2 <- m +
    prm_log_normal("cl") +
    input_variable("dose")
  expect_matching_issue(check(m), "Undefined variables")
  expect_no_matching_issue(check(m2), "Undefined variables")
})
