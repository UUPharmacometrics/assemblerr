
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

setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "ANY", component = "PrmLogNormal"),
  definition = function(target, source, component) {
    target <- target +
      nm_theta(component@name, lbound = 0) +
      nm_omega(component@name)

    theta_index <- index_of(target@facets$NmThetaParameterFacet, component@name)
    eta_index <- index_of(target@facets$NmOmegaParameterFacet, component@name)

    if (target@options$prm.use_mu_referencing) {
      mu_name <- sym(paste0("mu_",eta_index))
      target <- target + nm_pk(
        statement(
          !!mu_name <- log(theta[!!theta_index])
        )
      )
    }
    target +
      nm_pk(
        statement(
          !!sym(component@name) <- theta[!!theta_index]*exp(eta[!!eta_index])
        )
      )
  }
)


# normal ------------------------------------------------------------------


PrmNormal <- setClass("PrmNormal",
                      contains = "Parameter")

#' @export
prm_normal <- function(name) {
  PrmNormal(name = name)
}

setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "ANY", component = "PrmNormal"),
  definition = function(target, source, component) {
    target <- target +
      nm_theta(component@name, lbound = 0) +
      nm_omega(component@name)

    theta_index <- index_of(target@facets$NmThetaParameterFacet, component@name)
    eta_index <- index_of(target@facets$NmOmegaParameterFacet, component@name)
    if (target@options$prm.use_mu_referencing) {
      mu_name <- sym(paste0("mu_",eta_index))
      target <- target + nm_pk(
        statement(
          !!mu_name <- theta[!!theta_index]
        )
      )
    }
    target +
      nm_pk(
        statement(
          !!sym(component@name) <- theta[!!theta_index] + eta[!!eta_index]
        )
      )
  }
)

# logit ------------------------------------------------------------------


PrmLogitNormal <- setClass("PrmLogitNormal",
                      contains = "Parameter")

#' @export
prm_logit_normal <- function(name) {
  PrmLogitNormal(name = name)
}

setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "ANY", component = "PrmLogitNormal"),
  definition = function(target, source, component) {
    target <- target +
      nm_theta(component@name, lbound = 0, ubound = 1) +
      nm_omega(component@name)

    theta_index <- index_of(target@facets$NmThetaParameterFacet, component@name)
    eta_index <- index_of(target@facets$NmOmegaParameterFacet, component@name)
    if (target@options$prm.use_mu_referencing) {
      mu_name <- sym(paste0("mu_",eta_index))
      target <- target + nm_pk(
        statement(
          !!mu_name <- log(theta[!!theta_index])/(1 - log(theta[!!theta_index]))
        )
      )
    }
    target +
      nm_pk(
        statement(
          !!sym(paste0("logit_", component@name)) <- log(theta[!!theta_index])/(1 - log(theta[!!theta_index])) + eta[!!eta_index],
          !!sym(component@name) <- exp(!!sym(paste0("logit_", component@name)))/(1 + exp(!!sym(paste0("logit_", component@name))))
        )
      )
  }
)


# log-normal --------------------------------------------------------------



PrmNoVar <- setClass("PrmNoVar",
                         contains = "Parameter")

#' @export
prm_no_var <- function(name) {
  PrmNoVar(name = name)
}

setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "ANY", component = "PrmNoVar"),
  definition = function(target, source, component) {
    target <- target +
      nm_theta(component@name, lbound = 0)

    theta_index <- index_of(target@facets$NmThetaParameterFacet, component@name)

    target +
      nm_pk(
        statement(
          !!sym(component@name) <- theta[!!theta_index]
        )
      )
  }
)
