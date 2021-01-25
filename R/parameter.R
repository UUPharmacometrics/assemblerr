
Parameter <- setClass("Parameter",
                      contains = "NamedFacetEntry",
                      prototype = prototype(facet_class = "ParameterFacet"))

ParameterFacet <- setClass("ParameterFacet",
                           contains = "NamedFacet",
                           prototype = prototype(
                             entry_class = "Parameter",
                             label = "parameters"
                            ))

setMethod(
  f = "check",
  signature = signature(x = "ParameterFacet"),
  definition = function(x) {
    if (vec_is_empty(x@entries)) {
      CriticalIssue("No parameters specified")
    }else{
      NULL
    }
  }
)

assert_valid_parameter_name <- function(name) {
  if (!is.character(name) || !is_valid_variable_name(name)) {
    rlang::abort(
      c(
        "Invalid parameter name",
        i = "A parameter name can contain letters and numbers and needs to start with a letter"
      )
    )
  }
}

# log-normal --------------------------------------------------------------



PrmLogNormal <- setClass("PrmLogNormal",
                         contains = "Parameter")

#' @export
prm_log_normal <- function(name) {
  assert_valid_parameter_name(name)
  PrmLogNormal(name = name)
}

# normal ------------------------------------------------------------------


PrmNormal <- setClass("PrmNormal",
                      contains = "Parameter")

#' @export
prm_normal <- function(name) {
  assert_valid_parameter_name(name)
  PrmNormal(name = name)
}

# logit ------------------------------------------------------------------


PrmLogitNormal <- setClass("PrmLogitNormal",
                      contains = "Parameter")

#' @export
prm_logit_normal <- function(name) {
  assert_valid_parameter_name(name)
  PrmLogitNormal(name = name)
}

# no-var --------------------------------------------------------------

PrmNoVar <- setClass("PrmNoVar",
                         contains = "Parameter")

#' @export
prm_no_var <- function(name) {
  assert_valid_parameter_name(name)
  PrmNoVar(name = name)
}


