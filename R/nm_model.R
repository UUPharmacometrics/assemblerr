
#' NONMEM model
#'
#' \code{nm_model()} creates the foundation for a NONMEM model
#'
#' This function creates a NONMEM model object, a software-specific version of the general
#' \code{\link{model}}. Like for the general model,this function only creates the empty
#' base object which then needs to be filled with components before it can be rendered. The
#' following components can be added
#' to a NONMEM model:
#' \itemize{
#'    \item \code{\link{nm_problem}}
#'    \item \code{\link{nm_input}}
#'    \item \code{\link{nm_pk}}
#'    \item \code{\link{nm_des}}
#'    \item \code{\link{nm_error}}
#'    \item \code{\link{nm_theta}}
#'    \item \code{\link{nm_omega}}
#'    \item \code{\link{nm_sigma}}
#' }
#'
#' @return An nm_model
#' @export
#'
nm_model <- function(){
  structure(list(), class = c("nm_model", "comp_model", "fragment")) %>%
    add_facet("problem", list()) %>%
    add_facet("input", list(type = as.character(), properties = list())) %>%
    add_facet("subroutine", list()) %>%
    add_facet("pk", list(statement = list()), name_column = F) %>%
    add_facet("des", list(statement = list())) %>%
    add_facet("error", list(statement = list()), name_column = F) %>%
    add_facet("theta", list(initial = numeric(), lbound = numeric(), ubound = numeric())) %>%
    add_facet("omega", list(initial = numeric())) %>%
    add_facet("sigma", list(initial = numeric()))
}



#' Create NONMEM model facets
#'
#' @param name Facet name
#'
#' @return A facet
#' @export
nm_problem <- function(name){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("problem", name = name)
}

#' @export
#' @param type Column type
#' @param ... Additional arguments
#' @rdname nm_problem
nm_input <- function(name, type, ...){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("input", name = name, type = type, properties = list(...))
}


#' Create facet for initial values
#'
#' @param name Parameter name
#' @param initial Initial value
#' @param lbound Lower bound
#' @param ubound Upper bound
#'
#' @return Facet
#' @export
nm_theta <- function(name, initial = NA, lbound = -Inf, ubound = Inf){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("theta", name = name, initial = initial, lbound = lbound, ubound = ubound)
}
#' @export
#' @rdname nm_theta
nm_omega <- function(name, initial = NA){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("omega", name = name, initial = initial)
}
#' @export
#' @rdname nm_theta
nm_sigma <- function(name, initial = NA){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("sigma", name = name, initial = initial)
}


#' Create model code facet
#'
#' @param name Facet name
#' @param statement Code statement
#'
#' @return A facet
#' @export
nm_pk <- function(name, statement){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("pk", name = name, statement = statement)
}

#' @rdname  nm_pk
#' @export
nm_des <- function(name, statement){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("des", name = name, statement = statement)
}

#' @rdname  nm_pk
#' @export
nm_error <- function(name, statement){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("error", name = name, statement = statement)
}

