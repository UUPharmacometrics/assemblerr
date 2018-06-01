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

