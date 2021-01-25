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

setMethod(
  f = "check",
  signature = signature(x = "ObservationFacet"),
  definition = function(x) {
      if (vec_is_empty(x@entries)) {
        CriticalIssue("No observation specified")
      } else if (vec_size(x@entries) > 1) {
        CriticalIssue("More than one observation specified")
      }else{
        NULL
      }
    }
)


ObsNormalCombined = setClass("ObsNormalCombined",
         slots = c(prediction = "assemblerr_declaration", additive_term = "logical", proportional_term = "logical"),
         prototype = prototype(additive_term = TRUE, proportional_term = TRUE),
         contains = "Observation")

setMethod("initialize",
          signature = "ObsNormalCombined",
          definition = function(.Object, prediction = declaration(), ...){
            callNextMethod(.Object, prediction = prediction, ...)
          })

#' @export
obs_combined <- function(prediction, name) {
  prediction <- as_declaration(prediction)
  if (missing(name)) name <- dcl_make_obs_names(prediction)
  ObsNormalCombined(prediction = prediction, name = name)
}


# additive ----------------------------------------------------------------



AdditiveObservation <- setClass("AdditiveObservation",
                                contains = "ObsNormalCombined",
                                prototype = prototype(additive_term = TRUE, proportional_term = FALSE))


#' @export
obs_additive <- function(prediction, name) {
  prediction <- as_declaration(prediction)
  if (missing(name)) name <- dcl_make_obs_names(prediction)
  AdditiveObservation(prediction = prediction, name = name)
}



# proportional ------------------------------------------------------------



ProportionalObservation <- setClass("ProportionalObservation",
                                    contains = "ObsNormalCombined",
                                    prototype = prototype(additive_term = FALSE, proportional_term = TRUE))


#' @export
obs_proportional <- function(prediction, name) {
  prediction <- as_declaration(prediction)
  if (missing(name)) name <- dcl_make_obs_names(prediction)
  ProportionalObservation(prediction = prediction, name = name)
}


dcl_make_obs_names <- function(dcl) {
  ids <- dcl_id(dcl)
  is_null <- purrr::map_lgl(ids, is.null)
  obs_names <- as.character(ids)
  if (any(is_null)) {
    derived_names <- dcl_def(dcl)[is_null] %>%
      purrr::map_if(rlang::is_symbol, as.character) %>%
      purrr::map_if(expr_is_arr, ~paste0(.x[[2]], "_", .x[[3]]))
    is_complex_expr <- purrr::map_lgl(derived_names, purrr::negate(is.character))
    derived_names[is_complex_expr] <- paste0("obs", seq_len(sum(is_complex_expr)))
    obs_names[is_null] <- as.character(derived_names)
  }
  obs_names
}
