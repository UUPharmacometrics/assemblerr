
Parameter <- setClass("Parameter",
                      contains = "NamedFacetEntry",
                      prototype = prototype(facet_class = "ParameterFacet"))

ParameterFacet <- setClass("ParameterFacet",
                           contains = "NamedFacet",
                           prototype = prototype(
                             entry_class = "Parameter",
                             label = "parameters"
                            ))


# log-normal --------------------------------------------------------------



PrmLogNormal <- setClass("PrmLogNormal",
                         contains = "Parameter")

#' @export
prm_log_normal <- function(name) {
  PrmLogNormal(name = name)
}

# normal ------------------------------------------------------------------


PrmNormal <- setClass("PrmNormal",
                      contains = "Parameter")

#' @export
prm_normal <- function(name) {
  PrmNormal(name = name)
}

# logit ------------------------------------------------------------------


PrmLogitNormal <- setClass("PrmLogitNormal",
                      contains = "Parameter")

#' @export
prm_logit_normal <- function(name) {
  PrmLogitNormal(name = name)
}

# no-var --------------------------------------------------------------

PrmNoVar <- setClass("PrmNoVar",
                         contains = "Parameter")

#' @export
prm_no_var <- function(name) {
  PrmNoVar(name = name)
}


