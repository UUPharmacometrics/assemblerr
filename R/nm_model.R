
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
  new_fragment(
    facets = list(
      facet(facet_name = "problem", name = character()),
      facet(facet_name = "input", name = character(), type = character(), properties = list()),
      facet(facet_name = "subroutine", name = character()),
      facet(facet_name = "pk", statement = statement()),
      facet(facet_name = "des", name = character(), statement = statement()),
      facet(facet_name = "error", statement = statement()),
      facet(facet_name = "theta", name = character(), initial = numeric(), lbound = numeric(), ubound = numeric()),
      facet(facet_name = "omega", name = character(), initial = numeric()),
      facet(facet_name = "sigma", name = character(), initial = numeric())
    ),
    class = "nm_model")
}

setOldClass("nm_model")

#' Create NONMEM model facets
#'
#' @param name Facet name
#'
#' @return A facet
#' @export
nm_problem <- function(name){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  fragment(problem = list(name = name))
}

#' @export
#' @param type Column type
#' @param ... Additional arguments
#' @rdname nm_problem
nm_input <- function(name, type, ...){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  fragment(input = list(name = name, type = type, properties = list(NULL)))
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
nm_theta <- function(name, initial = 1.0, lbound = -Inf, ubound = Inf){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  fragment(theta = list(name = name, initial = initial, lbound = lbound, ubound = ubound))
}
#' @export
#' @rdname nm_theta
nm_omega <- function(name, initial = 0.1){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  fragment(omega = list(name = name, initial = initial))
}
#' @export
#' @rdname nm_theta
nm_sigma <- function(name, initial = 0.1){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  fragment(sigma = list(name = name, initial = initial))
}


#' Create model code facet
#'
#' @param name Facet name
#' @param statement Code statement
#'
#' @return A facet
#' @export
nm_pk <- function(name, statement){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  fragment(pk = list(statement = statement))
}

#' @rdname  nm_pk
#' @export
nm_des <- function(name, statement){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  fragment(des = list(name = name, statement = statement))
}

#' @rdname  nm_pk
#' @export
nm_error <- function(name, statement){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  fragment(error = list(statement = statement))
}

