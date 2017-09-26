#' @export
as_nmtran <- function(object, nm_model) UseMethod("as_nmtran")

#' @export
as_nmtran.Model <- function(model, nm_model){
  if(missing(nm_model) || is.null(nm_model))  nmtran_model <- NMModel$new()
  purrr::walk(model$parameters, ~as_nmtran(.x, nm_model = nmtran_model))
  return(nmtran_model)
}

#' @export
as_nmtran.LogNormalParameter <- function(parameter, nm_model){
  if(missing(nm_model) || is.null(nm_model))  nmtran_model <- NMModel$new()
  p_name <- parameter$name
  nm_model$add_parameter(name = p_name,
                         nm_model$add_theta(p_name), "*EXP(", nm_model$add_eta(p_name) ,")")
  return(nm_model)
}

