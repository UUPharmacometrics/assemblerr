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
parameter <- function(name, type, options = list()){
  if(name!=make.names(name)) stop("'name' needs to be a valid variable name.")
  if(missing(type)){
    message("No type for the parameter '", name,"' was specified, using 'log-normal' as default.")
    type <- "log_normal"
  }
  fragment(parameters = list(name = name, type = type, options = list(options)))
}

#' @export
#' @describeIn parameter Creates a parameter with a log-normal distribution
prm_log_normal <- function(name) {
  parameter(name, type = "log_normal")
}

#' @export
#' @describeIn parameter Creates a parameter with a normal distribution
prm_normal <- function(name) {
  parameter(name, type = "normal")
}

#' @export
#' @describeIn parameter Creates a parameter with no IIV
prm_novar <- function(name) parameter(name, type = "novar", values = check_pvs(values, c("mu")))

#' @export
#' @describeIn parameter Creates a parameter with a logit distribution
prm_logit <- function(name) parameter(name, type = "logit", values = values)


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

prmz_n_nonmem <- function(mu, sigma) return(c(mu, sigma^2))

add_prm_normal.nm_model <- function(target, source, prm){
  target <- target +
    nm_theta(prm$name, initial = NA) +
    nm_omega(prm$name, initial = NA)

  theta_index <-  get_by_name(target, "theta", prm$name)[["index"]]
  eta_index <- get_by_name(target, "omega", prm$name)[["index"]]

  target + nm_pk(name = prm$name,
                statement = statement(
                  !!sym(prm$name) <- theta[!!theta_index] + eta[!!eta_index]
                ))
}

add_prm_log_normal <- function(target, source, prm) UseMethod("add_prm_log_normal")


#' @export
ln_mean_sd <- function(mean, sd) return(c(log_mu = log(mean/sqrt(1+sd^2/mean^2)), log_sigma = sqrt(log(1+sd^2/mean^2))))

add_prm_log_normal.default <- function(target, source, prm) {
  rlang::warn("converter not implemented for this model type")
  return(target)
}

prmz_ln_nonmem <- function(log_mu, log_sigma) return(c(exp(log_mu), log_sigma^2))

add_prm_log_normal.nm_model <- function(target, source, prm){
  target <- target +
    nm_theta(prm$name, lbound = 0) +
    nm_omega(prm$name)

  theta_index <-  get_by_name(target, "theta", prm$name)[["index"]]
  eta_index <- get_by_name(target, "omega", prm$name)[["index"]]

  target + nm_pk(name = prm$name,
                statement = statement(
                  !!sym(prm$name) <- theta[!!theta_index]*exp(eta[!!eta_index])
                ))
}
