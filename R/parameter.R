
Parameter <- setClass("Parameter",
                      contains = "NamedFacetEntry",
                      slots = c(values = "numeric"),
                      prototype = prototype(facet_class = "ParameterFacet"))



ParameterFacet <- setClass("ParameterFacet",
                           contains = "NamedFacet",
                           prototype = prototype(
                             entry_class = "Parameter",
                             label = "parameters"
                            ))



setMethod(
  f = "check_component",
  signature = signature(x = "ParameterFacet"),
  definition = function(x, ...) {
    if (vec_is_empty(x@entries)) {
      CriticalIssue("No parameters specified")
    }else{
      NULL
    }
  }
)

setMethod(
  f = "compact_description",
  signature = "ParameterFacet",
  definition = function(x) {
    interp("parameters: {none(names(x@entries))}")
  }
)

assert_valid_parameter_name <- function(name) {
  if (rlang::is_missing(name)) {
    rlang::abort(
      c(
        "No parameter name provided",
        i = "Parameters need to be named."
      )
    )
  }
  if (!is.character(name) || !is_valid_variable_name(name)) {
    rlang::abort(
      c(
        "Invalid parameter name",
        i = "A parameter name can contain letters and numbers and needs to start with a letter."
      )
    )
  }
}

setMethod(
  f = "defined_variables",
  signature = "ParameterFacet",
  definition = function(x)  {
    purrr::map(x@entries, ~ParameterVariable(.x@name)) %>%
      purrr::reduce(.init = VariableList(), .f = combine)
  }
)

setMethod(
  f = "rename_variables",
  signature = "ParameterFacet",
  definition = function(x, variable_mapping) {
    for (i in seq_along(x@entries)) {
      if (x@entries[[i]]@name %in% names(variable_mapping)) {
        x@entries[[i]]@name <- variable_mapping[x@entries[[i]]@name]
      }
    }
    names(x@entries) <- names(x)
    return(x)
  }
)

# log-normal --------------------------------------------------------------



PrmLogNormal <- setClass("PrmLogNormal",
                         contains = "Parameter")


setMethod(
  f = "description",
  signature = "PrmLogNormal",
  definition = function(x) {
    interp("{x@name}: log-normal")
  }
)



#' Parameter with log-normal distribution
#'
#' This building block declares a parameter model for a parameter that follows the normal distribution on the log scale.
#'
#' @includeRmd man/rmd/parameter-model.Rmd
#'
#' @param name Parameter name
#' @param median Median (on the normal scale)
#' @param var_log Variance on the log scale
#'
#' @return A building block of type 'parameter'
#'
#' @family parameter models
#' @export
#' @examples
#' # EMAX dose-response model with emax (log-normal) and ed50 (no variability) parameters
#' m2 <- model() +
#'   input_variable("dose") +
#'   prm_log_normal("emax", 10, 0.3) +
#'   prm_no_var("ed50", 5) +
#'   obs_proportional(effect~emax*dose/(ed50+dose))
#'
#' # a log-normal parameter that is directly observed
#' m <- model() +
#'   prm_log_normal("wt") +
#'   obs_additive(~wt)
#'
prm_log_normal <- function(name, median = 1, var_log = 0.1) {
  assert_valid_parameter_name(rlang::maybe_missing(name))
  PrmLogNormal(name = name, values = c(median = median, var_log = var_log))
}

# normal ------------------------------------------------------------------


PrmNormal <- setClass("PrmNormal",
                      contains = "Parameter")

setMethod(
  f = "description",
  signature = "PrmNormal",
  definition = function(x) {
      interp("{x@name}: normal")
  }
)

#' Parameter with normal distribution
#'
#' This building block declares a parameter model for a parameter that follows the normal distribution.
#'
#' @includeRmd man/rmd/parameter-model.Rmd
#'
#' @param name Parameter name
#' @param mean Mean
#' @param var Variance
#'
#' @return A building block of type 'parameter'
#'
#' @family parameter models
#' @export
#' @inherit prm_log_normal examples
prm_normal <- function(name, mean = 1, var = 0.1) {
  assert_valid_parameter_name(rlang::maybe_missing(name))
  PrmNormal(name = name, values = c(mean = mean, var = var))
}

# logit ------------------------------------------------------------------


PrmLogitNormal <- setClass("PrmLogitNormal",
                      contains = "Parameter")

setMethod(
  f = "description",
  signature = "PrmLogitNormal",
  definition = function(x) {
    interp("{x@name}: logit-normal")
  }
)

#' Parameter with logit-normal distribution
#'
#' This building block declares a parameter model for a parameter that follows the normal distribution on the logit-scale.
#'
#' @includeRmd man/rmd/parameter-model.Rmd
#'
#' @param name Parameter name
#' @param mean_logit Mean on the logit scale
#' @param var_logit Variance on the logit scale
#'
#' @return A building block of type 'parameter'
#'
#' @family parameter models
#' @export
#' @inherit prm_log_normal examples
prm_logit_normal <- function(name, mean_logit = 0,  var_logit = 1) {
  assert_valid_parameter_name(rlang::maybe_missing(name))
  PrmLogitNormal(name = name, values = c(mean_logit = mean_logit, var_logit = var_logit))
}

# no-var --------------------------------------------------------------

PrmNoVar <- setClass("PrmNoVar",
                         contains = "Parameter")



setMethod(
  f = "description",
  signature = "PrmNoVar",
  definition = function(x) {
    interp("{x@name}: no variability")
  }
)

#' Parameter without variability
#'
#' This building block declares a parameter model for a parameter that does not vary between subjects.
#'
#' @includeRmd man/rmd/parameter-model.Rmd
#'
#' @param name Parameter name
#' @param value Parameter value
#'
#' @return A building block of type 'parameter'
#'
#' @family parameter models
#' @export
#' @inherit prm_log_normal examples
prm_no_var <- function(name, value = 1) {
  assert_valid_parameter_name(rlang::maybe_missing(name))
  PrmNoVar(name = name, values = c(value = value))
}


