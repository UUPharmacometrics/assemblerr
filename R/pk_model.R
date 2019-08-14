
#' Create a PK model
#'
#' @return A pk_model
#' @export
pk_model <- function(){
  model() %>%
    add_facet("pk_components", list(type = character(), options = list())) %>%
    rlang::set_attrs(class = c("pk_model", class(.)))
}

#' @export
as_model.pk_model <- function(from){
  model() %>%
    add_pk_components(from) %|+%
    obs_additive(name = "PK", ~A["central"]/vc) +
    purrr::update_list(from, pk_components = NULL) # all other components are taken "as-is"
}

#' Create PK model components
#'
#' @export
#' @param name Component name
#' @param type Component type
#' @param options Additional options
#' @return A pk_model component
pk_component <- function(name, type, options = NULL){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  if(!is.character(type)) stop("'type' needs to be a character vector")

  item("pk_components", name = name, type = type, options = options)
}

#' @export
#' @rdname pk_component
pk_distribution_1cmp <- function() pk_component("distribution", "1cmp")

#' @export
#' @rdname pk_component
pk_distribution_2cmp <- function() pk_component("distribution", "2cmp")

#' @rdname pk_component
#' @export
pk_distribution_3cmp <- function() pk_component("distribution", "3cmp")

#' @rdname pk_component
#' @export
pk_elemination_linear <- function() pk_component("elemination", "linear")

#' @rdname pk_component
#' @export
pk_elemination_mm <- function() pk_component("elemination", "mm")

#' @rdname pk_component
#' @export
pk_absorption_rate_fo <- function() pk_component("absorption-rate", "fo")

#' @rdname pk_component
#' @export
pk_absorption_rate_zo <- function() pk_component("absorption-rate", "zo")

#' @rdname pk_component
#' @export
pk_absorption_delay_none <- function() pk_component("absorption-delay", "none")


add_pk_components <- function(target, source){
  source$pk_components %>%
    purrr::transpose() %>%
    purrr::reduce(~call_pkc_converter(target = .x, source = source, pkc = .y), .init = target)
}

call_pkc_converter <- function(target, source, pkc) {
  # construct fn name
  fn_name <- paste("add", "pkc", pkc$type, sep = "_")
  # find function
  fn <- getFunction(fn_name, mustFind = F)
  #check if converter exists
  if(is.null(fn)) {
    rlang::warn("Converter not found")
    return(model)
  }
  # call converter
  do.call(fn, list(target, source, pkc))
}

add_pkc_1cmp <- function(target, source, pkc) {
  target +
    compartment(name = "central", volume = ~vc) +
    parameter("vc")
}

add_pkc_1cmp <- function(target, source, pkc) {
  target +
    compartment(name = "central", volume = ~vc) +
    parameter("vc")
}


add_pkc_2cmp <- function(target, source, pkc) {
    target +
      compartment(name = "central", volume = ~vc) +
      compartment(name = "peripheral", volume = ~vp) +
      flow(from = "central", to = "peripheral", definition = ~q*C) +
      flow(from = "peripheral", to = "central", definition = ~q*C) +
      parameter("vc") +
      parameter("vp") +
      parameter("q")
}

add_pkc_3cmp <- function(target, source, pkc){
    target +
      compartment(name = "central", volume = ~vc) +
      compartment(name = "peripheral1", volume = ~vp1) +
      compartment(name = "peripheral2", volume = ~vp2) +
      flow(from = "central", to = "peripheral1", definition = ~q1*C) +
      flow(from = "peripheral1", to = "central", definition = ~q1*C) +
      flow(from = "central", to = "peripheral2", definition = ~q2*C) +
      flow(from = "peripheral2", to = "central", definition = ~q2*C) +
      parameter("vc") +
      parameter("vp1") +
      parameter("vp2") +
      parameter("q1") +
      parameter("q2")
}


add_pkc_linear <- function(target, source, pkc) {
  target +
    parameter("cl") +
    flow(from = "central", definition = ~cl*C)
}

add_pkc_mm <- function(target, source, pkc) {
  target +
    parameter("vmax") +
    parameter("km")+
    flow(from = "central", definition = ~vmax*C/(km+C))
}

add_pkc_fo <- function(target, source, pkc){
    target +
      compartment("depot", volume = 1) +
      parameter("ka") +
      flow(from = "depot", "central", definition = ~ka*A)
}

add_pkc_zo <- function(target, source, pkc){
    target +
      compartment("depot", volume = 1) +
      parameter("k0") +
      flow(from = "depot", "central", definition = ~k0*A/(1E-6+A))
}

add_pkc_none <- function(target, source, pkc){
  target
}
