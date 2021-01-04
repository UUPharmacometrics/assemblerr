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


ObsNormalCombined = setClass("ObsNormalCombined",
         slots = c(prediction = "assemblerr_declaration", additive_term = "logical", proportional_term = "logical"),
         prototype = prototype(additive_term = TRUE, proportional_term = TRUE),
         contains = "Observation")

setMethod("initialize",
          signature = "ObsNormalCombined",
          definition = function(.Object, prediction = declaration(), ...){
            callNextMethod(.Object, name = dcl_id_label(prediction), prediction = prediction, ...)
          })

#' @export
obs_combined <- function(prediction) {
  ObsNormalCombined(prediction = as_declaration(prediction))
}


# additive ----------------------------------------------------------------



AdditiveObservation <- setClass("AdditiveObservation",
                                contains = "ObsNormalCombined",
                                prototype = prototype(additive_term = TRUE, proportional_term = FALSE))


#' @export
obs_additive <- function(prediction) {
  AdditiveObservation(prediction = as_declaration(prediction))
}



# proportional ------------------------------------------------------------



ProportionalObservation <- setClass("ProportionalObservation",
                                    contains = "ObsNormalCombined",
                                    prototype = prototype(additive_term = FALSE, proportional_term = TRUE))


#' @export
obs_proportional <- function(prediction) {
  ProportionalObservation(prediction = as_declaration(prediction))
}

