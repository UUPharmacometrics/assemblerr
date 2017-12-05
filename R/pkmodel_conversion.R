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
                  function(model, pk_component){
                    title <- get_by_name(model, "meta_tags", "title")$value
                    call_converter(facet = "pk_components",
                                   name  = c(pk_component$name, pk_component$type),
                                   from = from,
                                   to = model,
                                   fragment = pk_component) %+!%
                        meta_tag("title", paste0(title, pk_component$type, " "))
                  })
}

add_converter(
  facet = "pk_components",
  name = c("absorption-rate", "first-order"),
  target = "model",
  converter_fn = function(to, from, component){
    to +
      compartment("depot", volume = 1) +
      parameter("ka") +
      flow(from = "depot", "central", equation = ~ka*A)
  }
)

add_converter(
  facet = "pk_components",
  name = c("absorption-rate", "zero-order"),
  target = "model",
  converter_fn = function(to, from, component){
    to +
      compartment("depot", volume = 1) +
      parameter("k0") +
      flow(from = "depot", "central", equation = ~k0*A/(1E-6+A))
  }
)

add_converter(
  facet = "pk_components",
  name = c("absorption-delay", "none"),
  target = "model",
  converter_fn =  function(to, from, component){
    to
  }
)

add_converter(
  facet = "pk_components",
  name = c("absorption-delay", "alag"),
  target = "model",
  converter_fn =  function(to, from, component){
    to +
      parameter("alag2")
  }
)

add_converter(
  facet = "pk_components",
  name = c("elimination", "linear"),
  target = "model",
  converter_fn =  function(to, from, component){
    to +
      parameter("cl") +
      flow(from = "central", equation = ~cl*C)
  }
)

add_converter(
  facet = "pk_components",
  name = c("elimination", "mm"),
  target = "model",
  converter_fn =  function(to, from, component){
    to +
      parameter("vmax") +
      parameter("km")+
      flow(from = "central", equation = ~vmax*C/(km+C))
  }
)

add_converter(
  facet = "pk_components",
  name = c("distribution", "one-compartment"),
  target = "model",
  converter_fn =  function(to, from, component){
    to +
      compartment(name = "central", volume = ~vc) +
      parameter("vc")
  }
)

add_converter(
  facet = "pk_components",
  name = c("distribution", "two-compartment"),
  target = "model",
  converter_fn =  function(to, from, component){
    to +
      compartment(name = "central", volume = ~vc) +
      compartment(name = "peripheral", volume = ~vp) +
      flow(from = "central", to = "peripheral", equation = ~q*C) +
      flow(from = "peripheral", to = "central", equation = ~q*C) +
      parameter("vc") +
      parameter("vp") +
      parameter("q")
  }
)

add_converter(
  facet = "pk_components",
  name = c("distribution", "three-compartment"),
  target = "model",
  converter_fn =  function(to, from, component){
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
)
