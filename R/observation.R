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
  definition = function(x, ...) {
      if (vec_is_empty(x@entries)) {
        CriticalIssue("No observation specified")
      } else if (vec_size(x@entries) > 1) {
        CriticalIssue("More than one observation specified")
      }else{
        NULL
      }
    }
)

setMethod(
  f = "compact_description",
  signature = "ObservationFacet",
  definition = function(x) {
    cdesc <- purrr::map_chr(x@entries, compact_description)
    interp("observations: {none(cdesc)}")
  }
)


ObsNormalCombined = setClass("ObsNormalCombined",
         slots = c(prediction = "assemblerr_declaration", additive_term = "logical", proportional_term = "logical"),
         prototype = prototype(additive_term = TRUE, proportional_term = TRUE),
         contains = "Observation")

setMethod(
  f = "description",
  signature = "ObsNormalCombined",
  definition = function(x) {
      interp("{x@name}: ", format(x@prediction))
  }
)

setMethod(
  f = "compact_description",
  signature = "ObsNormalCombined",
  definition = function(x) {
    interp("{format(x@prediction)} (combined)")
  }
)

setMethod("initialize",
          signature = "ObsNormalCombined",
          definition = function(.Object, prediction = declaration(), ...){
            callNextMethod(.Object, prediction = prediction, ...)
          })

#' Observation model
#'
#' These building blocks specify an observational model describing the observed variable as well as how an observation is expected to diverge from
#' the model (i.e, the residual unexplained variability model).
#'
#' The RUV model is choosen by selecting one of the predefined functions.
#' \itemize{
#'   \item \code{obs_additive}: Observation following an additive error model \eqn{y = f + \epsilon_1}
#'   \item \code{obs_proportional}: Observation following a proportional error model \eqn{y = f + f \epsilon_1}
#'   \item \code{obs_combined}: Observation following a combined error model \eqn{y = f + f \epsilon_1 + \epsilon_2}
#' }
#' The first argument of these functions requires the definition of the actual prediction from the model. It can be specified in a number of different ways
#' \itemize{
#'   \item the name of a variable in the model: \code{obs_additive("effect")}
#'   \item a compartment concentration: \code{obs_additive(~C["central"])}
#'   \item an equation: \code{obs_additive(~base+slp*time)}
#' }
#' If the definition contains a variable name on the left-hand side (as in \code{conc~C["central"]}), the variable will appear in the generated model code.
#' The latter is useful to make the model code more readable if the prediction is defined as a long equation.
#'
#' The second function argument "name" will be used to support multiple observations but is currently not useful.
#'
#' @param prediction A declaration defining the observed variable
#' @param name A name for the observation (automatically derived if missing)
#'
#' @examples
#' # additve RUV model for observing the variable WT
#' m <- model() +
#'   prm_log_normal("wt") +
#'   obs_additive(~wt)
#'
#' # EMAX dose-response model with proportional RUV
#' m2 <- model() +
#'   input_variable("dose") +
#'   prm_no_var("emax") +
#'   prm_no_var("ed50") +
#'   obs_proportional(effect~emax*dose/(ed50+dose))
#' @name observation-model
NULL

#' @export
#' @rdname observation-model
#' @order 3
obs_combined <- function(prediction, name) {
  validate_and_create_observation(
    constructor = ObsNormalCombined,
    prediction = prediction,
    name = rlang::maybe_missing(name)
  )
}

validate_and_create_observation <- function(constructor, prediction, name) {
  prediction <- ui_as_declaration(prediction)
  if (rlang::is_missing(name)) name <- dcl_make_obs_names(prediction)
  assert_valid_observation_name(name)
  constructor(prediction = prediction, name = name)
}


# additive ----------------------------------------------------------------


AdditiveObservation <- setClass("AdditiveObservation",
                                contains = "ObsNormalCombined",
                                prototype = prototype(additive_term = TRUE, proportional_term = FALSE))

setMethod(
  f = "compact_description",
  signature = "AdditiveObservation",
  definition = function(x) {
    interp("{format(x@prediction)} (additive)")
  }
)

#' @export
#' @rdname observation-model
#' @order 1
obs_additive <- function(prediction, name) {
  validate_and_create_observation(
    constructor = AdditiveObservation,
    prediction = prediction,
    name = rlang::maybe_missing(name)
  )
}



# proportional ------------------------------------------------------------



ProportionalObservation <- setClass("ProportionalObservation",
                                    contains = "ObsNormalCombined",
                                    prototype = prototype(additive_term = FALSE, proportional_term = TRUE))

setMethod(
  f = "compact_description",
  signature = "ProportionalObservation",
  definition = function(x) {
    interp("{format(x@prediction)} (proportional)")
  }
)


#' @export
#' @rdname observation-model
#' @order 2
obs_proportional <- function(prediction, name) {
  validate_and_create_observation(
    constructor = ProportionalObservation,
    prediction = prediction,
    name = rlang::maybe_missing(name)
  )
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

assert_valid_observation_name <- function(name) {
  if (!is.character(name) || !is_valid_variable_name(name)) {
    rlang::abort(
      c(
        "Invalid observation name",
        i = "An observation name can contain letters and numbers and needs to start with a letter."
      )
    )
  }
}
