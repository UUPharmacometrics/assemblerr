#' Model parameter
#'
#' Defines name and type of a model parameter
#'
#' @param name Name of the paramter
#' @param type Model type to be used for the parameter
#' @param options Options
#'
#' @return A \code{fragment} representing a model parameter
#' @export
#' @examples
#' p <- parameter("cl", "log-normal")
parameter <- function(name, type, options = NULL){
  if(name!=make.names(name)) stop("'name' needs to be a valid variable name.")
  if(missing(type)){
    message("No type for the parameter '", name,"' was specified, using 'log-normal' as default.")
    type <- "log_normal"
  }
  item("parameters", name = name, type = type, options = options)
}

#' @export
#' @describeIn parameter Creates a parameter with a log-normal distribution
prm_log_normal <- function(name) parameter(name, type = "log_normal")

#' @export
#' @describeIn parameter Creates a parameter with a normal distribution
prm_normal <- function(name) parameter(name, type = "normal")

#' @export
#' @describeIn parameter Creates a parameter with no IIV
prm_novar <- function(name) parameter(name, type = "novar")

#' @export
#' @describeIn parameter Creates a parameter with a logit distribution
prm_logit <- function(name) parameter(name, type = "logit")


add_parameters <- function(target, source){
  source$parameters %>%
    purrr::transpose() %>%
    purrr::reduce(~call_prm_converter(target = .x, source = source, prm = .y), .init = target)
}

call_prm_converter <- function(target, source, prm) {
  # construct fn name
  fn_name <- paste("add", "prm", prm$type, sep = "_")
  # find function
  fn <- getFunction(fn_name, mustFind = F)
  #check if converter exists
  if(is.null(fn)) {
    rlang::warn("Converter not found")
    return(model)
  }
  # call converter
  do.call(fn, list(target, source, prm))
}


add_prm_normal <- function(target, source, prm) UseMethod("add_prm_normal")

add_prm_normal.default <- function(target, source, prm) {
  rlang::warn("converter not implemented for this model type")
  return(target)
}

add_prm_normal.nm_model <- function(target, source, prm){
  taret <- target +
    nm_theta(prm$name) +
    nm_omega(prm$name)

  theta_index <-  get_by_name(target, "theta", prm$name)$index
  eta_index <- get_by_name(target, "omega", prm$name)$index

  expr <- bquote(.(rlang::sym(prm$name)) <- theta[.(theta_index)]+eta[.(eta_index)])

  target + nm_pk(name = prm$name,
                statement = expr)
}

add_prm_log_normal <- function(target, source, prm) UseMethod("add_prm_log_normal")

add_prm_log_normal.default <- function(target, source, prm) {
  rlang::warn("converter not implemented for this model type")
  return(target)
}

add_prm_log_normal.nm_model <- function(target, source, prm){
  target <- target +
    nm_theta(prm$name) +
    nm_omega(prm$name)

  theta_index <-  get_by_name(target, "theta", prm$name)$index
  eta_index <- get_by_name(target, "omega", prm$name)$index

  expr <- bquote(.(rlang::sym(prm$name)) <- theta[.(theta_index)]*exp(eta[.(eta_index)]))

  target + nm_pk(name = prm$name,
                statement = expr)
}
