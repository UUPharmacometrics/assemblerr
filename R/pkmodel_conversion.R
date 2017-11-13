#' @export
as_model <- function(from) UseMethod("as_model")

#' @export
as_model.pk_model <- function(from){

 model() %>%
    convert_pk_components(from) %|+%
    observation(equation = ~A["central"]/vc, type = "additive") +
    purrr::update_list(from, pk_components = NULL)
}

convert_pk_components <- function(to, from){
  from$pk_components %>%
    purrr::transpose() %>%
    purrr::reduce(.init = to,
                  ~convert_pk_component(.x, from, .y))
}


convert_pk_component <- function(to, from, component){
  .pk_component_conversion[[component$name]][[component$type]](to, from, component)
}

.pk_component_conversion <- list()
.pk_component_conversion[["absorption-rate"]] <- list()
.pk_component_conversion[["absorption-rate"]][["first-order"]] <- function(to, from, component){
  to +
    compartment("depot", volume = 1) +
    parameter("ka") +
    flow(from = "depot", "central", equation = ~ka*A)
}
.pk_component_conversion[["absorption-rate"]][["zero-order"]] <- function(to, from, component){
  to +
    compartment("depot", volume = 1) +
    parameter("k0") +
    flow(from = "depot", "central", equation = ~k0*A/(1E-6+A))
}
.pk_component_conversion[["absorption-delay"]] <- list()
.pk_component_conversion[["absorption-delay"]][["none"]] <- function(to, from, component){
  to
}
.pk_component_conversion[["absorption-delay"]][["alag"]] <- function(to, from, component){
  to +
    parameter("alag2")
}
.pk_component_conversion[["elimination"]] <- list()
.pk_component_conversion[["elimination"]][["linear"]] <- function(to, from, component){
  to +
    parameter("cl") +
    flow(from = "central", equation = ~cl*C)
}
.pk_component_conversion[["elimination"]][["mm"]] <- function(to, from, component){
  to +
    parameter("vmax") +
    parameter("km")+
    flow(from = "central", equation = ~vmax*C/(km+C))
}

.pk_component_conversion[["distribution"]] <- list()
.pk_component_conversion[["distribution"]][["one-compartment"]] <- function(to, from, component){
  to +
    compartment(name = "central", volume = ~vc) +
    parameter("vc")
}
.pk_component_conversion[["distribution"]][["two-compartment"]] <- function(to, from, component){
  to +
    compartment(name = "central", volume = ~vc) +
    compartment(name = "peripheral", volume = ~vp) +
    flow(from = "central", to = "peripheral", equation = ~q*C) +
    flow(from = "peripheral", to = "central", equation = ~q*C) +
    parameter("vc") +
    parameter("vp") +
    parameter("q")
}
.pk_component_conversion[["distribution"]][["three-compartment"]] <- function(to, from, component){
  to +
    compartment(name = "central", volume = ~vc) +
    compartment(name = "peripheral1", volume = ~vp1) +
    compartment(name = "peripheral2", volume = ~vp2) +
    flow(from = "central", to = "peripheral1", equation = ~q1*C) +
    flow(from = "peripheral1", to = "central", equation = ~q1*C) +
    flow(from = "central", to = "peripheral2", equation = ~q2*C) +
    flow(from = "peripheral2", to = "central", equation = ~q2*C) +
    parameter("vc") +
    parameter("vp1") +
    parameter("vp2") +
    parameter("q1") +
    parameter("q2")
}
