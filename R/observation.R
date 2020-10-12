
setClass("ModelObservation",
         slots = c(name = "character"), contains = "list")
setClass("NormalObservation",
         slots = c(mu = "assemblerr_declaration", sigma = "assemblerr_declaration"),
         contains = "ModelObservation")
setClass("AdditiveObservation",
         contains = "NormalObservation")
setMethod(f = "initialize",
          signature = "AdditiveObservation",
          definition = function(.Object, prediction = declaration(), ...){
            callNextMethod(.Object,
                           mu = prediction,
                           sigma = declaration(~1),
                           ...)
          })

setClass("ProportionalObservation",
         contains = "NormalObservation")

setGeneric(name = "convert",
           def = function(target, source, component) standardGeneric("convert"))



#' Observation model
#'
#' Defines how variables from a model relate to values in the data
#'
#' @param name The name used to identify the measurement
#' @param type The model type used for the observation model
#' @param options A formula describing the measurement
#'
#' @return A \code{fragment} representing an observation model
#'
#' @examples
#' # create an additive error observation model for the concentration from the "central" compartment
#' c_obs <- observation(name = "conc", type = "additive", options = list(prediction = ~C["central"]))
#' @export
observation <- function(name, type, values, options = NULL){
  if(missing(type)) {
    message("No type for the observation model was specified, using 'additive' as a default")
    type <- "additive"
  }
  if(!is.character(type)) stop("'type' needs to be a character vector")
  if(name!=make.names(name)) stop("'name' needs to be a valid variable name")
  item("observations", name = name, type = type, values = list(values), options = options)
}

obs_continuous <- function(prediction, type, name, values){
  prediction <- arg2fml(prediction)
  if(missing(name)) name <- fml_get_lhs(prediction) %>% deparse()
  options <- list(
    prediction = prediction
  )
  observation(name, type, values, options)
}

#' @export
#' @param prediction Declaration for prediction
#' @rdname observation
obs_additive <- function(prediction, name) {
  fragment(observations = list(name = name, type = list(new("AdditiveObservation", prediction = as_declaration(prediction)))))
}

#' @export
#' @rdname observation
obs_proportional <- function(prediction, name) {
  obs_continuous(prediction, "proportional", name, values = check_pvs(values, c("sigma"), lower = c(sigma = 0)))
}

#' @export
#' @rdname observation
obs_power <- function(prediction, name) {
  obs_continuous(prediction, "power", name)
}

#' @export
#' @param p1 Declaration for P(Y=1)
#' @rdname observation
obs_binary <- function(p1, name){
  p1 <- arg2fml(p1)
  options <- list(
    p1 = p1
  )
  observation(name, "binary", options)
}


add_observations <- function(target, source){
  source$observations$type %>%
    purrr::reduce(~convert(.x, source, .y), .init = target)
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

generate_conc_fml <- function(cmp) {
  bquote(C[.(cmp$name)]~A[.(cmp$name)]/volume) %>%
  fml_subs_sym(volume = fml_get_rhs(cmp$volume))
}

# replace_compartment_references <- function(fml, to, from){
#   if(any(c("C","A") %in% fml_vars(fml))){
#     compartment_indicies <- purrr::set_names(to$des$index, to$des$name)
#     # generate replacement rules for concentration
#     conc_fmls <- from$compartments %>%
#       purrr::transpose() %>%
#       purrr::map(generate_conc_fml)
#
#     tfml <- purrr::exec(.fn = fml_subs_fml, !!!conc_fmls, fml = fml) %>%
#       fml_subs_idx("A", compartment_indicies)
#     return(tfml)
#   }else{
#     return(fml)
#   }
# }

replace_compartment_references <- function(dcl, target, source){
  if (any(c("C","A") %in% dcl_vars_chr(dcl))) {
    conc_dcls <- as.list(generate_concentration_substitutions(source$compartments))
    compartment_indicies <- purrr::set_names(target$des$index, target$des$name)
    dcl <- dcl %>%
      dcl_substitute(substitutions = conc_dcls) %>%
      dcl_substitute_index("A", compartment_indicies)
  }
  return(dcl)
}

make_ipred_fml <- function(target, source, obs){

  ipred_fml <- replace_compartment_references(obs$options$prediction, target, source)
  if(fml_is_anonymous(ipred_fml)){
    ipred_fml <- list(fml_set_lhs(ipred_fml, quote(ipred)))
  }else{
    ipred_fml <- list(ipred_fml, bquote(ipred~.(fml_get_lhs(ipred_fml))))
  }
  return(ipred_fml)
}

add_obs_additive <- function(target, source, obs) UseMethod("add_obs_additive")

add_obs_additive.default <- function(target, source, obs) {
  rlang::warn("converter not implemented for this model type")
}

prmz_obs_add_nonmem <- function(sigma) return(c(sigma^2))

add_obs_additive.nm_model <- function(target, source, obs){
  pv <- to_prmz(prmz_obs_add_nonmem, obs)

  # generate a name for sigma
  sigma_name <- c("ruv", obs$name, "add") %>%
    purrr::discard(~.x=="") %>%
    paste0(collapse="-")
  target <- target + nm_sigma(sigma_name, pv)

  # create the declartions for this observations
  obs_fml <- make_ipred_fml(target, source, obs)
  ruv_fml <- bquote(y~ipred+eps[.(get_by_name(target, "sigma", sigma_name)$index)])
  obs_fml[[length(obs_fml)+1]] <- ruv_fml

  # convert to statements for NM
  obs_expr <- as_expr(obs_fml)

  # add conditional statement if there is more than one observation
  # if(nrow(source$observations)>1) {
  #   obs_stm <- stm(if(dvid == !!(obs$index)){
  #     !!!(obs_stm$expressions)
  #   })
  # }

  target + nm_error(name = obs$name, statement = obs_expr)
}

generate_concentration_substitutions <- function(cmps){
  d <- dcl_substitute(declaration(C[name]~A[name]),
                      substitutions = list(name = cmps$name))
  dcl_devide(d, cmps$volume)
}

setMethod(
  f = "convert",
  signature = c(target = "nm_model", source = "ANY", component = "AdditiveObservation"),
  definition = function(target, source, component) {
    target <- target + nm_sigma("sigma")
    ipred_dcl <- component@mu
    if (vec_size(source$compartments) > 0) {
      ipred_dcl <- replace_compartment_references(ipred_dcl, target, source)
    }
    dcl_id(ipred_dcl) <- quote(ipred)
    ruv_dcl <- dcl_multiply(component@sigma, declaration(~eps[1]))
    d <- vec_c(ipred_dcl,
               dcl_add(declaration(y~ipred), ruv_dcl))
    target <- target + nm_error("", statement = as_statement(d))
    target
  }
)

setMethod(
  f = "convert",
  signature = c(target = "nm_model", source = "ANY", component = "ProportionalObservation"),
  definition = function(target, source, component) {
    print("proportional not implemented")
  }
)
