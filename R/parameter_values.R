


# parameter value should actually be requested during the conversion of the parameter model. The software-specific
# parameter model should request the desired parameter (e.g. typical) and the parameter value system should
# return the requested value, doing eventual conversions if needed.

#' Get parameter values
#'
#' @param m Model
#' @param prm_name Parameter name
#' @param prmz Parameterization
#'
#' @return
get_pv_log_normal <- function(m, prm_name, prmz){
  # collect all parameter value information for specific parameter
  pvs <- get_all(m, "parameter_values", parameter == prm_name) %>%
    purrr::map("values") %>%
    purrr::flatten_dbl()
  # convert provided prm values to std parameterization

  # possible input parameterizations (https://sites.google.com/site/probonto/download)
  input_prmz <- list(
    function(log_mu, log_sigma, ...) return(list(log_mu = log_mu,
                                         log_sigma = log_sigma,
                                         not_used = list(...))),
    function(mean, sd, ...) return(list(log_mu = log(mean/sqrt(1+sd^2/mean^2)),
                                        log_sigma = sqrt(log(1+sd^2/mean^2)),
                                        not_used = list(...)))
  )

  # safely evaluate each input paramterization with the list of pvs
  conversion_results <- purrr::map(input_prmz, purrr::safely) %>%
    purrr::map(~rlang::exec(.x, !!!pvs))

  # retrieve successful conversions
  successful_conversion <- conversion_results %>%
    purrr::map("result") %>%
    purrr::compact()

  if(rlang::is_empty(successful_conversion)) {
    ui_warning("Did not find matching parameter values.")
    return(c(NA,NA))
  }
  if(length(successful_conversion)>1) {
    ui_warning("Multiple parameter values.")
  }
  std_pvs <- successful_conversion[[1]]
  # warn about unused values
  if(!rlang::is_empty(std_pvs$not_used))
    ui_warning("Parameter values ", paste(names(std_pvs$not_used), collapse = ","),
               " provided for parameter '", prm_name,"' not required and ignored.")
  std_pvs$not_used <- NULL
  # convert from std prmz to requested one
  output_prmzs <- list(
    "log_mu-log_sigma" = function(log_mu, log_sigma) return(c(log_mu = log_mu, log_sigma = log_sigma)),
    "log_mu-log_sigma2" = function(log_mu, log_sigma) return(c(log_mu = log_mu, log_sigma2 = log_sigma^2)),
    "mean-sd" = function(log_mu, log_sigma) return(c(mean = exp(log_mu+0.5*log_sigma^2),
                                                 sd = exp(log_mu+0.5*log_sigma^2)*sqrt(exp(log_sigma^2-1))))
  )
  if(!exists(prmz, output_prmzs)) {
    ui_warning("Requested output parameterization not available.")
    return(c(NA,NA))
  }
  rlang::exec(output_prmzs[[prmz]], !!!std_pvs)

}

