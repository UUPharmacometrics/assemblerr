

test_that("combining of issues", {
  expect_equal(c(IssueList(), Issue("test"), CriticalIssue("test2")),  IssueList(Issue("test"), CriticalIssue("test2")))
})


test_that("printing of issues", {
  local_edition(3)
  local_reproducible_output()
  issues <- IssueList(Issue("test"), CriticalIssue("test2"))
  expect_snapshot(print(issues))
})
