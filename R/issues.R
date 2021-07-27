
Issue <- setClass("Issue", contains = "character")

CriticalIssue <- setClass("CriticalIssue",
                          contains = "Issue")

MissingVariableIssue <- setClass("MissingVariableIssue",
                                 slots = c(variables = "character"),
                          contains = "CriticalIssue")

ChangedVariableCapitalizationIssue <- setClass("ChangedVariableCapitalizationIssue",
                                       slots = c(variables = "character"),
                                       contains = "Issue")

setMethod(f = "initialize",
          signature = "ChangedVariableCapitalizationIssue",
          definition = function(.Object, vars, ...){
            msg <- interp("Same variables with different capitalization ({vars})")
            callNextMethod(.Object, msg, variables = vars)
          })

IssueList <- setClass("IssueList", contains = "list")

setMethod(f = "initialize",
          signature = "IssueList",
          definition = function(.Object, ...){
            entries <- rlang::dots_list(...)
            callNextMethod(.Object, entries)
          })

validIssueList <- function(object) {
  if (vec_is_empty(object@.Data) || all(purrr::map_lgl(object@.Data, ~is(.x, "Issue")))) return(TRUE)
  "All entries need to be of class 'Issue'"
}

setValidity("IssueList", validIssueList)

#' Combine issues
#' @param x An IssueList
#' @param ... objects to add to the issue list
#' @keywords internal
setMethod(
  f = "c",
  signature = "IssueList",
  definition = function(x, ...) {
    new_entries <- rlang::dots_list(...) %>%
      purrr::map_if(~is(.x, "list"), ~.x@.Data) %>%
      purrr::map_if(~is(.x, "character"), ~list(.x)) %>%
      purrr::flatten()
    IssueList(!!!vec_c(x@.Data, new_entries))
  }
)

setMethod(
  f = "show",
  signature = "IssueList",
  definition = function(object) {
    if (vec_is_empty(object@.Data)) {
      cli::cli_alert_success("No issues")
    } else{
      n_issues <- length(object)
      n_critical <- length(purrr::keep(object, ~is(.x, "CriticalIssue")))
      n_non_critical <- n_issues - n_critical
      div_id <- cli::cli_div(theme = list(
        "li" = list("margin-left" = 2),
        ".critical" = list(color = "red"),
        ".non-critical" = list(color = "black")
        )
      )
      if (n_issues == n_critical) {
        cli::cli_text(cli::symbol$warning, " {n_issues} {.critical critical} issue{?s}")
      } else if (n_critical == 0) {
        cli::cli_text(cli::symbol$warning, " {n_issues} {.non-critical non-critical} issue{?s}")
      } else {
        cli::cli_text(cli::symbol$warning, " {n_issues} issue{?s} ({.critical {n_critical} critical} & {.non-critical {n_non_critical} non-critical})")
      }
      ol_id <- cli::cli_ol()
      purrr::walk(sort(object), ~cli::cli_li(.x, class = ifelse(is(.x, "CriticalIssue"), "critical", "non-critical")))
      cli::cli_end(ol_id)
      cli::cli_end(div_id)
    }
    invisible(NULL)
  }
)



#' Auxiliary Function for Sorting and Ranking
#'
#' @param x an R Object
#'
#' @keywords internal
setMethod(
  f = "xtfrm",
  signature = "IssueList",
  definition = function(x) {
    purrr::map_lgl(x, ~!is(.x, "CriticalIssue")) %>%
      as.integer()
  }
)

setGeneric(
  name = "issue_types",
  def = function(x) standardGeneric("issue_types")
)

setMethod(
  f = "issue_types",
  signature = "IssueList",
  definition = function(x) purrr::map_chr(x, class)
)


setGeneric(
  name = "discard_issues",
  def = function(x, class) standardGeneric("discard_issues")
)

setMethod(
  f = "discard_issues",
  signature = "IssueList",
  definition = function(x, class) purrr::discard(x, ~is(.x, class)) %>%
    as("IssueList")
)

#' Checking for issues
#'
#' This function checks a model for existing issues.
#'
#' The function accepts a model object and returns a list of issues that can help to identify problems in a model.
#' If no issues are found, a message and an empty list are produced. Issues can either be critical or non-critical,
#' depending on whether a valid model could still be rendered.
#'
#' The function currently detects the following issues:
#'   - Undefined variables
#'   - Lack of parameters
#'   - Lack of observations
#'   - Lack of distribution/elimination components (pk_model)
#'   - Inconsistent capitalization of variable names
#'
#' @param model Model to check
#' @return An issue list (printed to the console by default)
#' @md
#' @examples
#' m <- model() +
#'     prm_log_normal("emax") +
#'     prm_log_normal("ed50") +
#'     obs_additive(eff~emax*dose/(ed50+dose))
#' check(m)
#'
#' # fix issue
#' m <- m + input_variable("dose")
#' check(m)
#' @export
check <- function(model) {
  check_component(model)
}
