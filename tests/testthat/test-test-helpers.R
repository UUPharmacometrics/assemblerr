test_that("creation of dummy data", {
  dir <- local_create_nonmem_test_directory()
  m <- model() +
    prm_log_normal("v") +
    prm_log_normal("cl") +
    obs_additive(conc~amt/v*exp(-cl/v*time))
  create_dummy_data(m, path = "data.csv")
  expect_true(file.exists(file.path(dir, "data.csv")))
})

test_that("does_not_contain expectation", {
  expect_success(expect_does_not_contain(c("test", "test2"), "test3"))
  expect_failure(expect_does_not_contain(c("test", "test2"), "test2"))
})

test_that("expect_matching_issue helper", {
  issues <- IssueList(
    Issue("test issue")
  )
  expect_success(
    expect_matching_issue(issues, "test")
  )
  expect_failure(
    expect_matching_issue(issues, "missing")
  )
})

test_that("expect_no_matching_issue helper", {
  issues <- IssueList(
    Issue("test issue")
  )
  expect_success(
    expect_no_matching_issue(issues, "missing")
  )
  expect_failure(
    expect_no_matching_issue(issues, "test")
  )
})



test_that("simple model helper", {
  m <- simple_model(prm = prm_normal("test"))
  expect_s4_class(m, "Model")
  expect_s4_class(m@facets$ParameterFacet@entries[[1]], "PrmNormal")
})
