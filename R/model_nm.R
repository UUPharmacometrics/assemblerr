#' Create NONMEM model
#'
#' @return An empty NONMEM model
#' @export
model_nm <- function(){
  structure(list(), class = c("model_nm", "model", "fragment")) %>%
    add_facet("thetas", list(initial = numeric(), lbound = numeric(), ubound = numeric())) %>%
    add_facet("omegas", list(initial = numeric())) %>%
    add_facet("sigmas", list(initial = numeric())) %>%
    add_facet("odes", list(equation = list())) %>%
    add_facet("parameter_equations", list(equation = list())) %>%
    add_facet("pk_variables", list(equation = list())) %>%
    add_facet("observation_equations", list(ipred_equation = list(), ruv_equation = list())) %>%
    add_facet("data_items", list())
}
#' @export
theta <- function(name, initial = NA, lbound = -Inf, ubound = Inf){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("thetas", name = name, initial = initial, lbound = lbound, ubound = ubound)
}
#' @export
omega <- function(name, initial = NA){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("omegas", name = name, initial = initial)
}
#' @export
sigma <- function(name, initial = NA){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("sigmas", name = name, initial = initial)
}


#' Create a new ODE
#'
#' @param name
#' @param equation
#'
#' @return An ODE
#' @export
ode <- function(name, equation){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  if(!is_equationish(equation)) stop("'equation' needs to be interpretable as an equation")
  item("odes", name = name, equation = as_equation(equation))
}
#' @export
parameter_equation <- function(name, equation){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  if(!is_equationish(equation)) stop("'equation' needs to be interpretable as an equation")
  item("parameter_equations", name = name, equation = as_equation(equation))
}
#' @export
observation_equation <- function(name, ipred_equation, ruv_equation){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  if(!is_equationish(ipred_equation)) stop("'ipred_equation' needs to be interpretable as an equation")
  if(!is_equationish(ruv_equation)) stop("'ruv_equation' needs to be interpretable as an equation")
  item("observation_equations", name = name, ipred_equation = as_equation(ipred_equation), ruv_equation = as_equation(ruv_equation))
}
#' @export
data_item <- function(name){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("data_items", name = name)
}

#' @export
data_items <- function(names){
  item("data_items", name = names)
}

#' @export
#' @keywords internal
pk_variable <- function(name, equation){
  item("pk_variables", name = name, equation = equation)
}
