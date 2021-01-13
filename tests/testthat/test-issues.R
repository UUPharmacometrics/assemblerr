
# ensure consisten output on CI system
setup(cli::start_app(theme = list(".alert-warning" = list(before = "WARNING! "))))
teardown(cli::stop_app())


test_that("combining of issues", {
  expect_equal(c(IssueList(), Issue("test"), CriticalIssue("test2")),  IssueList(Issue("test"), CriticalIssue("test2")))
})


test_that("printing of issues", {
  local_edition(3)
  local_reproducible_output()
  issues <- IssueList(Issue("test"), CriticalIssue("test2"))
  expect_snapshot(print(issues))
})
