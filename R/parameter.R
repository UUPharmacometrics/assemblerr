
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
#' Parameter model
#'
#' These building blocks define a parameter model, declaring a parameter and specifying how it varies between subjects.
#'
#' The parameter building blocks require a name for the parameter by which it can be referenced in a different building
#' block. The distribution of the parameter in the population is chosen by selecting among the following predefined
#'  functions:
#'   * `prm_no_var`: A parameter without variability, i.e., without random effects.
#'   * `prm_normal`: A parameter following the normal distribution.
#'   * `prm_log_normal`: A parameter following the log-normal distribution.
#'   * `prm_logit_normal`: A parameter following the logit-normal distribution (the parameter is normally distributed on the logit scale).
#'
#' Adding a parameter with an already existing name will replace the definition of the parameter. For example, the parameter "base"
#' will have a log-normal distribution in the following snippet:
#' ```
#' m <- model() +
#'   prm_normal("base") +
#'   prm_log_normal("base")
#' ```
#'
#' ## MU-referencing
#'
#' `assemblerr` can include mu-referencing statements for parameter distributions that support it. The functionality can be
#' activated by setting the option `prm.use_mu_referencing` to `TRUE` as shown in the following snippet:
#' ```
#' m <- model() +
#'    prm_normal("base") +
#'    prm_log_normal("slp") +
#'    obs_additive(response~base+slp*time)
#'
#' render(
#'   model = m,
#'   options = assemblerr_options(prm.use_mu_referencing = TRUE)
#' )
#' ```
#'
#'
#' @param name A name for the parameter
#'
#' @examples
#' # a log-normal parameter that is directly observed
#' m <- model() +
#'   prm_log_normal("wt") +
#'   obs_additive(~wt)
#'
#' # EMAX dose-response model with emax and ed50 parameters
#' m2 <- model() +
#'   input_variable("dose") +
#'   prm_no_var("emax") +
#'   prm_no_var("ed50") +
#'   obs_proportional(effect~emax*dose/(ed50+dose))
#' @name parameter-model
NULL
#> NULL

#' @export
#' @rdname parameter-model
#' @order 3
#' @md
prm_log_normal <- function(name) {
  assert_valid_parameter_name(name)
  PrmLogNormal(name = name)
}

# normal ------------------------------------------------------------------


PrmNormal <- setClass("PrmNormal",
                      contains = "Parameter")

#' @export
#' @rdname parameter-model
#' @order 2
prm_normal <- function(name) {
  assert_valid_parameter_name(name)
  PrmNormal(name = name)
}

# logit ------------------------------------------------------------------


PrmLogitNormal <- setClass("PrmLogitNormal",
                      contains = "Parameter")

#' @export
#' @rdname parameter-model
#' @order 4
prm_logit_normal <- function(name) {
  assert_valid_parameter_name(name)
  PrmLogitNormal(name = name)
}

# no-var --------------------------------------------------------------

PrmNoVar <- setClass("PrmNoVar",
                         contains = "Parameter")

#' @export
#' @rdname parameter-model
#' @order 1
prm_no_var <- function(name) {
  assert_valid_parameter_name(name)
  PrmNoVar(name = name)
}


