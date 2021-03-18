
Issue <- setClass("Issue", contains = "character")

CriticalIssue <- setClass("CriticalIssue",
                          contains = "Issue")

MissingVariableIssue <- setClass("MissingVariableIssue",
                                 slots = c(variables = "character"),
                          contains = "CriticalIssue")


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
      cli::cli_alert_warning("{length(object)} issue{?s}")
      div_id <- cli::cli_div(theme = list(
        "ol" = list("margin-left" = 2),
        ".critical" = list(color = "red"),
        ".non-critical" = list(color = "black")
        )
      )
      cli::cli_ol(purrr::map_chr(object, as.character), class = "critical")
      cli::cli_end(div_id)
    }
    invisible(NULL)
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
