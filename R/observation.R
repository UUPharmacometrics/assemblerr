#' @include facet.R
#' @include declaration.R

Observation <-  setClass("Observation",
         contains = "NamedFacetEntry",
         prototype = prototype(facet_class = "ObservationFacet"))


ObservationFacet <- setClass("ObservationFacet",
                           contains = "NamedFacet",
                           prototype = prototype(
                             entry_class = "Observation",
                             label = "observations"
                          ))


setClass("ObsNormal",
         slots = c(mu = "assemblerr_declaration", sigma = "assemblerr_declaration"),
         contains = "Observation")



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
obs_proportional <- function(prediction) {
  ProportionalObservation(prediction = as_declaration(prediction))
}



