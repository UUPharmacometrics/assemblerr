
setClass("PkComponent",
         slots = c(prm_names = "character"), contains = "list")

setClass("PkDistributionComponent",
         contains = "PkComponent")

setClass("PkEliminationComponent",
         contains = "PkComponent")

setClass("Pk")
# distribution 1 cmp ------------------------------------------------------


setClass("PkDistribution1Cmp",
         contains = "PkDistributionComponent")

setMethod(
  f = "convert",
  signature = c(target = "model", source = "ANY", component = "PkDistribution1Cmp"),
  definition = function(target, source, component) {
    target +
      compartment(name = "central", volume = as_declaration(component@prm_names['vc']))
  }
)

# distribution 2 cmp ------------------------------------------------------


setClass("PkDistribution2Cmp",
         contains = "PkDistributionComponent")

setMethod(
  f = "convert",
  signature = c(target = "model", source = "ANY", component = "PkDistribution2Cmp"),
  definition = function(target, source, component) {
    dcl_q <- dcl_substitute(declaration(~q*C), substitutions = list(q = sym(component@prm_names['q'])))
    target +
      compartment(name = "central", volume = as_declaration(component@prm_names['vc'])) +
      compartment(name = "peripheral", volume =  as_declaration(component@prm_names['vp'])) +
      flow(from = "central", to = "peripheral", definition = dcl_q) +
      flow(from = "peripheral", to = "central", definition = dcl_q)
  }
)




# elimination linear ------------------------------------------------------


setClass("PkEliminationLinear",
         contains = "PkEliminationComponent")

setMethod(
  f = "convert",
  signature = c(target = "model", source = "ANY", component = "PkEliminationLinear"),
  definition = function(target, source, component) {
    dcl <- declaration(~cl*C) %>%
      dcl_substitute(list(cl = sym(component@prm_names['cl'])))
    target +
      flow(from = "central", definition = dcl)
  }
)



#' Create a PK model
#'
#' @return A pk_model
#' @export
pk_model <- function(){
  new_fragment(
    facets = list(
      facet(facet_name = "pk_components", name = character(), type = list()),
      facet(facet_name = "parameters", name = character(), type = character(), values = list(), options = list()),
      facet(facet_name = "algebraics", name = character(), definition = declaration()),
      facet(facet_name = "observations", name = character(), type = list())
    ),
    class = c("pk_model", "model")
  )
}

setOldClass("pk_model")

#' @export
as_model.pk_model <- function(from){
  model() %>%
    add_pk_components(from)  +
    purrr::update_list(from, pk_components = NULL) # all other components are taken "as-is"
}

#' Create PK model components
#'
#' @export
#' @param name Component name
#' @param type Component type
#' @return A pk_model component
pk_component <- function(name, class, prm_names){
  stopifnot(is.character(name))
  stopifnot(is.character(class))
  stopifnot(is.character(prm_names))

  obj <- new(class, prm_names = prm_names)
  fragment(pk_components = list(name = name, type = list(obj)))
}

#' @export
#' @rdname pk_component
pk_distribution_1cmp <- function(prm_vc = prm_log_normal("vc")) {
  prm_names <- c(vc = prm_vc[[1]]$name)
  pk_component("distribution", "PkDistribution1Cmp", prm_names = prm_names) +
    prm_vc
}

#' @export
#' @rdname pk_component
pk_distribution_2cmp <- function(
  prm_vc = prm_log_normal("vc"),
  prm_vp = prm_log_normal("vp"),
  prm_q = prm_log_normal("q")
) {
  prm_names <- c(vc = prm_vc[[1]]$name,
                 vp = prm_vp[[1]]$name,
                 q = prm_q[[1]]$name)
  pk_component("distribution", "PkDistribution2Cmp", prm_names = prm_names) +
    prm_vc +
    prm_vp +
    prm_q
}

#' @rdname pk_component
#' @export
pk_distribution_3cmp <- function() pk_component("distribution", "3cmp")

#' @rdname pk_component
#' @export
pk_elemination_linear <- function(prm_cl = prm_log_normal("cl")) {
  prm_names <- c(cl = prm_cl[[1]]$name)
  pk_component("elimination", "PkEliminationLinear", prm_names = prm_names) +
    prm_cl
}

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
  source$pk_components$type %>%
    purrr::reduce(~convert(.x, source, .y), .init = target)
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
