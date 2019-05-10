#' Observation model
#'
#' Defines how variables from a model relate to values in the data
#'
#' @param name The name used to identify the measurement
#' @param type The model type used for the observation model
#' @param options A \code{\link{declaration}} describing the measurement
#'
#' @return A \code{\link{fragment}} representing an observation model
#'
#' @examples
#' # create an additive error observation model for the concentration from the "central" compartment
#' c_obs <- observation(conc~C["central"], "additive")
#'
#' # create a combined error observation model "eff" for the variable effect
#' e_obs <- observation(name = "eff", ~effect, "combined")
#' @export
observation <- function(name, type, options = NULL){
  if(missing(type)) {
    message("No type for the observation model was specified, using 'additive' as a default")
    type <- "additive"
  }
  if(!is.character(type)) stop("'type' needs to be a character vector")
  if(name!=make.names(name)) stop("'name' needs to be a valid variable name")
  item("observations", name = name, type = type, options = options)
}

obs_continuous <- function(prediction, type, name){
  prediction <- arg2dec(prediction)
  if(missing(name)) name <- dec_get_id(prediction) %>% as.character()
  options <- list(
    prediction = prediction
  )
  observation(name, type, options)
}

#' @export
obs_additive <- function(prediction, name) obs_continuous(prediction, "additive", name)

#' @export
obs_proportional <- function(prediction, name) obs_continuous(prediction, "proportional", name)

#' @export
obs_power <- function(prediction, name) obs_continuous(prediction, "power", name)

#' @export
obs_binary <- function(p1, name){
  p1 <- arg2dec(p1)
  options <- list(
    p1 = p1
  )
  observation(name, "binary", options)
}


add_observations <- function(target, source){
  source$observations %>%
    purrr::transpose() %>%
    purrr::reduce(~call_obs_converter(target = .x, source = source, obs = .y), .init = target)
}

call_obs_converter <- function(target, source, obs) {
  # construct fn name
  fn_name <- paste("add", "obs", obs$type, sep = "_")
  # find function
  fn <- getFunction(fn_name, mustFind = F)
  #check if converter exists
  if(is.null(fn)) {
    rlang::warn("Converter not found")
    return(model)
  }
  # call converter
  do.call(fn, list(target, source, obs))
}

replace_compartment_references <- function(d, to, from){
  if(any(c("C","A") %in% dec_vars(d))){
    compartment_indicies <- to$des %>%
    {
      purrr::set_names(as.list(.$index), .$name)
    }
    # generate replacement rules for concentration
    conc_declarations <- from$compartments %>%
      purrr::transpose() %>%
      purrr::map(~declaration(C[!!(.x$name)], A[!!(.x$name)]/vol) %>% dec_subs(dec_set_id(.x$volume, vol)))

    dt <- d %>%
      purrr::invoke(.f = dec_subs, .x = conc_declarations, d = .) %>%
      dec_index_subs("A", compartment_indicies)
    return(dt)
  }else{
    return(d)
  }
}

make_ipred_dec <- function(target, source, obs){

  ipred_dec <- replace_compartment_references(obs$options$prediction, target, source)
  if(is_anonymous(ipred_dec)){
    ipred_dec <- list(dec_set_id(ipred_dec, ipred))
  }else{
    ipred_dec <- list(ipred_dec, declaration("ipred", !!(dec_get_id(ipred_dec))))
  }
  return(ipred_dec)
}

add_obs_additive <- function(target, source, obs) UseMethod("add_obs_additive")

add_obs_additive.default <- function(target, source, obs) {
  rlang::warn("converter not implemented for this model type")
}

add_obs_additive.nm_model <- function(target, source, obs){
  # generate a name for sigma
  sigma_name <- c("ruv", obs$name, "add") %>%
    purrr::discard(~.x=="") %>%
    paste0(collapse="-")
  target <- target + nm_sigma(sigma_name)

  # create the declartions for this observations
  obs_dec <- make_ipred_dec(target, source, obs)
  obs_dec[[length(obs_dec)+1]] <- declaration(y, ipred + eps[!!(get_by_name(target, "sigma", sigma_name)$index)])

  # convert to statements for NM
  obs_stm <- as_statement(obs_dec)

  # add conditional statement if there is more than one observation
  if(nrow(source$observations)>1) {
    obs_stm <- stm(if(dvid == !!(obs$index)){
      !!!(obs_stm$expressions)
    })
  }

  target + nm_error(name = obs$name, statement = obs_stm)
}
