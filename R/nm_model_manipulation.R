#' Create NONMEM model
#'
#' @return An empty NONMEM model
#' @export
create_nm_model <- function(){
  structure(list(), class = "nm_model") %>%
    add_facet("thetas", list(initial = numeric(), lbound = numeric(), ubound = numeric())) %>%
    add_facet("omegas", list(initial = numeric())) %>%
    add_facet("sigmas", list(initial = numeric())) %>%
    add_facet("compartments", list(equation = list())) %>%
    add_facet("parameters", list(equation = list())) %>%
    add_facet("observations", list(ipred_equation = list(), ruv_equation = list()))
}


add_compartment.nm_model <- function(model, name, equation) add_entry(model, "compartments", list(name = name, equation = list(equation)))

add_parameter.nm_model <- function(model, name, equation) add_entry(model, "parameters", list(name = name, equation = list(as_equation(equation))))

add_observation.nm_model <- function(model, name, ipred_equation, ruv_equation) add_entry(model, "observations", list(name = name, ipred_equation = list(as_equation(ipred_equation)),
                                                                                                                      ruv_equation = list(as_equation(ruv_equation))))

add_theta <- function(model, ...) UseMethod("add_theta")

add_theta.nm_model <- function(model, name) add_entry(model, "thetas", list(name = name))

add_omega <- function(model, ...) UseMethod("add_omega")

add_omega.nm_model <- function(model, name) add_entry(model, "omegas", list(name = name))

add_sigma <- function(model, ...) UseMethod("add_sigma")

add_sigma.nm_model <- function(model, name) add_entry(model, "sigmas", list(name = name))
