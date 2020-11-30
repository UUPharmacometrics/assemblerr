#' @include facet.R
#' @include declaration.R

Observation <-  setClass("Observation",
         contains = "NamedFacetEntry",
         prototype = prototype(facet_class = "ObservationFacet"))


ObservationFacet <- setClass("ObservationFacet",
                           contains = "NamedFacet",
                           prototype = prototype(entry_class = "Observation"))


setClass("ObsNormal",
         slots = c(mu = "assemblerr_declaration", sigma = "assemblerr_declaration"),
         contains = "Observation")

setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "ANY", component = "ObsNormal"),
  definition = function(target, source, component) {
    target <- target + nm_sigma("sigma")
    ipred_dcl <- component@mu
    if (vec_size(source@facets$CompartmentFacet@entries) > 0) {
      ipred_dcl <- replace_compartment_references(ipred_dcl, target, source)
    }
    ruv_dcl <- dcl_multiply(component@sigma, declaration(~eps[1]))
    y_dcl <- declaration(y ~ ipred) %>%
      dcl_substitute(
        list(
          ipred = dcl_id(ipred_dcl)
        )
      )
    d <- vec_c(ipred_dcl,
               dcl_add(y_dcl, ruv_dcl))
    target <- target + nm_error(statement = as_statement(d))
    target
  }
)



# additive ----------------------------------------------------------------



AdditiveObservation <- setClass("AdditiveObservation", contains = "ObsNormal")
setMethod(f = "initialize",
          signature = "AdditiveObservation",
          definition = function(.Object, prediction = declaration(), ...){
            callNextMethod(.Object,
                           mu = prediction,
                           sigma = declaration(~1),
                           name = dcl_id_label(prediction),
                           ...)
          })

#' @export
#' @param prediction Declaration for prediction
obs_additive <- function(prediction) {
  AdditiveObservation(prediction = as_declaration(prediction))
}



# proportional ------------------------------------------------------------



ProportionalObservation <- setClass("ProportionalObservation", contains = "ObsNormal")

setMethod(f = "initialize",
          signature = "ProportionalObservation",
          definition = function(.Object, prediction = declaration(), ...){
            callNextMethod(.Object,
                           mu = prediction,
                           sigma = as_declaration(dcl_id_label(prediction)),
                           name = dcl_id_label(prediction),
                           ...)
          })

#' @export
#' @param prediction Declaration for prediction
obs_proportional <- function(prediction) {
  ProportionalObservation(prediction = as_declaration(prediction))
}




replace_compartment_references <- function(dcl, target, source){
  if (any(c("C","A") %in% dcl_vars_chr(dcl))) {
    conc_dcls <- as.list(generate_concentration_substitutions(source@facets[["CompartmentFacet"]]@entries))
    compartment_indicies <- name_index_map(target@facets$NmCompartmentFacet)
    dcl <- dcl %>%
      dcl_substitute(substitutions = conc_dcls) %>%
      dcl_substitute_index("A", compartment_indicies) %>%
      dcl_substitute_index("dadt", compartment_indicies)
  }
  return(dcl)
}


generate_concentration_substitutions <- function(cmps){
  names <- names(cmps)
  volume <- vec_c(!!!unname(purrr::map(cmps, "volume")))
  d <- dcl_substitute(declaration(C[name]~A[name]),
                      substitutions = list(name = names))
  dcl_devide(d, volume)
}

