#' @include parameter.R


# log-normal --------------------------------------------------------------


setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "ANY", component = "PrmLogNormal"),
  definition = function(target, source, component, options) {
    target <- target +
      nm_theta(component@name, lbound = 0) +
      nm_omega(component@name)

    theta_index <- index_of(target@facets$NmThetaParameterFacet, component@name)
    eta_index <- index_of(target@facets$NmOmegaParameterFacet, component@name)

    if (options$prm.use_mu_referencing) {
      mu_name <- sym(paste0("mu_",eta_index))
      target <- target + nm_pk(
        statement(
          bquote(.(mu_name) <- log(theta[.(theta_index)]))
        )
      )
    }
    target +
      nm_pk(
        statement(
          bquote(.(sym(component@name)) <- theta[.(theta_index)]*exp(eta[.(eta_index)]))
        )
      )
  }
)


# normal ------------------------------------------------------------------



setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "ANY", component = "PrmNormal"),
  definition = function(target, source, component, options) {
    target <- target +
      nm_theta(component@name, lbound = 0) +
      nm_omega(component@name)

    theta_index <- index_of(target@facets$NmThetaParameterFacet, component@name)
    eta_index <- index_of(target@facets$NmOmegaParameterFacet, component@name)
    if (options$prm.use_mu_referencing) {
      mu_name <- sym(paste0("mu_",eta_index))
      target <- target + nm_pk(
        statement(
          bquote(.(mu_name) <- theta[.(theta_index)])
        )
      )
    }
    target +
      nm_pk(
        statement(
          bquote(.(sym(component@name)) <- theta[.(theta_index)] + eta[.(eta_index)])
        )
      )
  }
)

# logit ------------------------------------------------------------------



setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "ANY", component = "PrmLogitNormal"),
  definition = function(target, source, component, options) {
    target <- target +
      nm_theta(component@name, lbound = 0, ubound = 1) +
      nm_omega(component@name)

    theta_index <- index_of(target@facets$NmThetaParameterFacet, component@name)
    eta_index <- index_of(target@facets$NmOmegaParameterFacet, component@name)
    if (options$prm.use_mu_referencing) {
      mu_name <- sym(paste0("mu_",eta_index))
      target <- target + nm_pk(
        statement(
          bquote(.(mu_name) <- log(theta[(theta_index)])/(1 - log(theta[.(theta_index)])))
        )
      )
    }
    target +
      nm_pk(
        statement(
          bquote(.(sym(paste0("logit_", component@name))) <- log(theta[.(theta_index)])/(1 - log(theta[.(theta_index)])) + eta[.(eta_index)]),
          bquote(.(sym(component@name)) <- exp(.(sym(paste0("logit_", component@name))))/(1 + exp(.(sym(paste0("logit_", component@name))))))
        )
      )
  }
)


# no-var --------------------------------------------------------------


setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "ANY", component = "PrmNoVar"),
  definition = function(target, source, component, options) {
    target <- target +
      nm_theta(component@name, lbound = 0)

    theta_index <- index_of(target@facets$NmThetaParameterFacet, component@name)

    target +
      nm_pk(
        statement(
          bquote(.(sym(component@name)) <- theta[.(theta_index)])
        )
      )
  }
)
