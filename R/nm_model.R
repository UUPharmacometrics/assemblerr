
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

#' A problem
#'
#'
#' @export
#' @keywords internal
nm_problem <- function(name){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("problem", name = name)
}

#' @export
nm_input <- function(name, type, ...){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("input", name = name, type = type, properties = list(...))
}


#' @keywords internal
nm_theta <- function(name, initial = NA, lbound = -Inf, ubound = Inf){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("theta", name = name, initial = initial, lbound = lbound, ubound = ubound)
}
#' @keywords internal
nm_omega <- function(name, initial = NA){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("omega", name = name, initial = initial)
}
#' @keywords internal
nm_sigma <- function(name, initial = NA){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("sigma", name = name, initial = initial)
}
#' @keywords internal
nm_pk <- function(name, statement){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("pk", name = name, statement = statement)
}
#' @keywords internal
nm_des <- function(name, statement){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("des", name = name, statement = statement)
}
#' @keywords internal
nm_error <- function(name, statement){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("error", name = name, statement = statement)
}

