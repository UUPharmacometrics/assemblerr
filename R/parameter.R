#' Model parameter
#'
#' Defines name and type of a model parameter
#'
#' @param name Name of the paramter
#' @param type Model type to be used for the parameter
#'
#' @return A \code{\link{fragment}} representing a model parameter
#' @export
#' @examples
#' p <- parameter("cl", "log-normal")
parameter <- function(name, type, options = NULL){
  if(name!=make.names(name)) stop("'name' needs to be a valid variable name.")
  if(missing(type)){
    message("No type for the parameter '", name,"' was specified, using 'log-normal' as default.")
    type <- "log-normal"
  }
  item("parameters", name = name, type = type, options = options)
}

#' @export
prm_log_normal <- function(name) parameter(name, type = "log-normal")

#' @export
prm_normal <- function(name) parameter(name, type = "normal")

#' @export
prm_novar <- function(name) parameter(name, type = "novar")
