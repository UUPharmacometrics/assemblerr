#' @include parameter.R


# log-normal --------------------------------------------------------------


setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "ANY", component = "PrmLogNormal"),
  definition = function(target, source, component, options) {
    values <- parameter_values(component)
    target <- target +
      nm_theta(component@name, initial = values["median"], lbound = 0) +
      nm_omega(component@name, initial = values["var_log"])

    theta_index <- index_of(target@facets$NmThetaParameterFacet, component@name)
    eta_index <- index_of(target@facets$NmOmegaParameterFacet, component@name)

    if (options$prm.use_mu_referencing) {
      mu_name <- paste0('mu_',eta_index)
      stm <- statement(
        "{mu_name} <- log(theta[{theta_index}])",
        "{component@name} <- exp({mu_name} + eta[{eta_index}])"
      )
    } else {
      stm <- statement(
        "{component@name} <- theta[{theta_index}]*exp(eta[{eta_index}])"
      )
    }
    target + nm_pk(stm)
  }
)


setMethod(
  f = "convert",
  signature = c(target = "NmModel2", source = "ANY", component = "PrmLogNormal"),
  definition = function(target, source, component, options) {
    values <- parameter_values(component)
    target <- target +
      nm_theta_record(component@name, initial = values["median"], lbound = 0) +
      nm_omega_record(component@name, initial = values["var_log"])

    theta_index <- index_of(target@thetas, component@name)
    eta_index <- index_of(target@omegas, component@name)

    if (options$prm.use_mu_referencing) {
      mu_name <- paste0('mu_',eta_index)
      stm <- statement(
        "{mu_name} <- log(theta[{theta_index}])",
        "{component@name} <- exp({mu_name} + eta[{eta_index}])"
      )
    } else {
      stm <- statement(
        "{component@name} <- theta[{theta_index}]*exp(eta[{eta_index}])"
      )
    }
    target + nm_pk_code(stm)
  }
)


# normal ------------------------------------------------------------------



setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "ANY", component = "PrmNormal"),
  definition = function(target, source, component, options) {
    values <- parameter_values(component)
    target <- target +
      nm_theta(component@name, initial = values["mean"], lbound = 0) +
      nm_omega(component@name, initial = values["var"])

    theta_index <- index_of(target@facets$NmThetaParameterFacet, component@name)
    eta_index <- index_of(target@facets$NmOmegaParameterFacet, component@name)
    if (options$prm.use_mu_referencing) {
      mu_name <- sym(paste0("mu_",eta_index))
      stm <- statement(
        "{mu_name} <- theta[{theta_index}]",
        "{component@name} <- {mu_name} + eta[{eta_index}]"
      )
    } else {
      stm <- statement(
        "{component@name} <- theta[{theta_index}] + eta[{eta_index}]"
      )
    }
    target + nm_pk(stm)
  }
)

# logit ------------------------------------------------------------------



setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "ANY", component = "PrmLogitNormal"),
  definition = function(target, source, component, options) {
    values <- parameter_values(component)
    target <- target +
      nm_theta(component@name, initial = values["mean_logit"], lbound = 0, ubound = 1) +
      nm_omega(component@name, initial = values["var_logit"])

    theta_index <- index_of(target@facets$NmThetaParameterFacet, component@name)
    eta_index <- index_of(target@facets$NmOmegaParameterFacet, component@name)
    prm_logit <- paste0("logit_", component@name)
    prm <- component@name
    if (options$prm.use_mu_referencing) {
      mu_name <- paste0('mu_',eta_index)
      stm <- statement(
        "{mu_name} <- log(theta[{theta_index}])/(1 - log(theta[{theta_index}]))",
        "{prm_logit} <- {mu_name} + eta[{eta_index}]",
        "{prm} <- exp({prm_logit})/(1 + exp({prm_logit}))"
      )
    } else {
      stm <- statement(
        "{prm_logit} <- log(theta[{theta_index}]/(1 - theta[{theta_index}])) + eta[{eta_index}]",
        "{prm} <- exp({prm_logit})/(1 + exp({prm_logit}))"
      )
    }
    target + nm_pk(stm)
  }
)


# no-var --------------------------------------------------------------


setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "ANY", component = "PrmNoVar"),
  definition = function(target, source, component, options) {
    values <- parameter_values(component)
    target <- target +
      nm_theta(component@name, initial = values["value"], lbound = 0)

    theta_index <- index_of(target@facets$NmThetaParameterFacet, component@name)

    target +
      nm_pk(
        statement(
          "{component@name} <- theta[{theta_index}]"
        )
      )
  }
)
