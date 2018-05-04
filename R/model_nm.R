
#' NONMEM model
#'
#' \code{model_nm()} creates the foundation for a NONMEM model
#'
#' This function creates a NONMEM model object, a software-specific version of the general \link{\code{model_nm()}}. Like for the general model,
#' this function only creates the empty base object which then needs to be filled with components before it can be rendered. The following components can be added
#' to a NONMEM model:
#' \itemize{
#'    \item \code{\link{theta}}
#'    \item \code{\link{omega}}
#'    \item \code{\link{sigma}}
#'    \item \code{\link{flow}}
#'    \item \code{\link{ode}}
#'    \item \code{\link{parameter_equation}}
#'    \item \code{\link{algebraic_equation}}
#'    \item \code{\link{observation_equation}}
#'    \item \code{\link{data_item}}
#'    \item \code{\link{meta_tag}}
#' }
#'
#' @return
#' @export
#'
#' @examples
model_nm <- function(){
  structure(list(), class = c("model_nm", "model", "fragment")) %>%
    add_facet("thetas", list(initial = numeric(), lbound = numeric(), ubound = numeric())) %>%
    add_facet("omegas", list(initial = numeric())) %>%
    add_facet("sigmas", list(initial = numeric())) %>%
    add_facet("odes", list(equation = list())) %>%
    add_facet("parameter_equations", list(equation = list())) %>%
    add_facet("algebraic_equations", list(equation = list())) %>%
    add_facet("observation_declarations", list(declarations = list())) %>%
    add_facet("data_items", list(type = as.character())) %>%
    add_facet("meta_tags", list(value = as.character())) +
    data_items(c("ID", "TIME", "DV"), c("id", "idv", "dv"))
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
  equation <- arg_as_declaration(equation)
  item("odes", name = name, equation = equation)
}
#' @export
parameter_equation <- function(name, equation){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  equation <- arg_as_declaration(equation)
  item("parameter_equations", name = name, equation = equation)
}
#' @export
algebraic_equation <- function(name, equation){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  equation <- arg_as_declaration(equation)
  item("algebraic_equations", name = name, equation = equation)
}

#' @export
observation_declaration <- function(name, declarations){
  declarations <- arg_as_declaration_list(declarations)
  item("observation_declarations", name = name, declarations = declarations)
}


#' @export
data_item <- function(name, type){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("data_items", name = name, type = type)
}

#' @export
data_items <- function(names, types){
  item("data_items", name = names, type = types)
}

